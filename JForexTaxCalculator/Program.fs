open System
open System.Diagnostics
open System.Globalization
open System.IO
open System.Net.Http
open System.Threading.Tasks
open ConsoleTables
open Newtonsoft.Json

[<Literal>]
let nbpApiDayRangeLimit = 367

[<Literal>]
let beforeStartDayShift = 7

[<Literal>]
let closedPositionReportCellSeparator = ','

let decimalCulture = CultureInfo.InvariantCulture

type NbpExchangeRate =
    { No: string
      EffectiveDate: DateTime
      Mid: decimal }

type NbpExchangeRateDto =
    { Table: string
      Currency: string
      Code: string
      Rates: List<NbpExchangeRate> }

type PositionCloseReportRow =
    { CloseDate: DateTime
      PositionId: int64
      NetPl: decimal
      Currency: string }

let rec getDateRanges (startDate: DateTime) (endDate: DateTime) (dayShift: int) =
    seq {
        if startDate.AddDays(dayShift) >= endDate then
            yield (startDate, endDate)
        elif dayShift % nbpApiDayRangeLimit = 0 then
            yield (startDate, startDate.AddDays(dayShift))
            yield! getDateRanges (startDate.AddDays(float (dayShift + 1))) endDate 1
        else
            yield! getDateRanges startDate endDate (dayShift + 1)
    }

let getNbpRates (currencySymbol: string) (dateRanges: List<DateTime * DateTime>) =
    async {
        let httpClient = new HttpClient()

        let rec getRate accumulator (dateRanges: List<DateTime * DateTime>) =
            match dateRanges with
            | [] -> accumulator
            | (startDate, endDate) :: t ->
                let uri =
                    new Uri(
                        $"""http://api.nbp.pl/api/exchangerates/rates/A/{currencySymbol}/{startDate.ToString("yyyy-MM-dd")}/{endDate.ToString("yyyy-MM-dd")}/?format=json"""
                    )

                getRate (accumulator @ [ httpClient.GetStringAsync(uri) ]) t

        let getRateTasks = getRate [] dateRanges

        Task.WhenAll(getRateTasks) |> Async.AwaitTask |> ignore

        return getRateTasks |> List.map (fun x -> x.Result)
    }

let readClosedPositionReport (fileLines: List<string>) =
    let rec readLine accumulator (fileLines: List<string>) =
        match fileLines with
        | h :: t ->
            let cells = h.Split(closedPositionReportCellSeparator)

            if cells[0] |> String.IsNullOrEmpty || cells[1] |> String.IsNullOrEmpty then
                readLine accumulator t
            else
                readLine
                    ({ CloseDate = DateTime.Parse(cells[1])
                       Currency = cells[16]
                       NetPl = Decimal.Parse(cells[14], decimalCulture)
                       PositionId = Int64.Parse(cells[3]) }
                     :: accumulator)
                    t
        | _ -> accumulator
    
    readLine [] fileLines

let generateReport (closedPositionsReport : List<PositionCloseReportRow>) (nbpRates : List<NbpExchangeRate>) (currency : string) =
    let consoleTable = new ConsoleTable("Position Id",
        "Position close date",
        "NBP rate date",
        "NBP rate no",
        "NBP rate mid",
        $"Net P/L in {currency}",
        "Net P/L in PLN")
    
    let rec generateRow (closedPositionsReport : List<PositionCloseReportRow>) (nbpRates : List<NbpExchangeRate>) totalPl totalIncomePln totalCostsPln =
        match closedPositionsReport with
        | h :: t ->
            let rate = nbpRates
                    |> List.filter (fun q -> q.EffectiveDate < h.CloseDate)
                    |> List.sortByDescending (fun q -> q.EffectiveDate)
                    |> List.head
            
            let incomePln = if h.NetPl > 0m then h.NetPl * rate.Mid else 0m
            let costsPln = if h.NetPl < 0m then h.NetPl * rate.Mid else 0m
            
            consoleTable.AddRow(h.PositionId,
                h.CloseDate.ToString("yyyy-MM-dd"),
                rate.EffectiveDate.ToString("yyyy-MM-dd"),
                rate.No,
                rate.Mid,
                h.NetPl,
                h.NetPl * rate.Mid) |> ignore
            
            generateRow t nbpRates (totalPl + h.NetPl) (totalIncomePln + incomePln) (totalCostsPln + costsPln)
        | _ ->
            consoleTable.Write();
            Console.WriteLine();
            Console.WriteLine($"Total P/L {currency}: {totalPl}");
            Console.WriteLine($"Total income PLN: {totalIncomePln}");
            Console.WriteLine($"Total costs PLN: {totalCostsPln}");
            Console.WriteLine($"Net profit: {totalIncomePln - Math.Abs(totalCostsPln)}");
            Console.WriteLine($"Net profit rounded: {Math.Round(totalIncomePln - Math.Abs(totalCostsPln), 2, MidpointRounding.ToPositiveInfinity)}");
            ()
            
    generateRow closedPositionsReport nbpRates 0m 0m 0m
    ()

let args = Environment.GetCommandLineArgs()

if args.Length <> 2 then
    Console.WriteLine($"Usage: {Path.GetFileName(Process.GetCurrentProcess().MainModule.FileName)} positions-close.csv")
else
    let report =
        File.ReadAllLines(args[1])
        |> Array.skip 1
        |> Array.toList
        |> readClosedPositionReport

    let startDate = (report |> List.map (fun q -> q.CloseDate) |> List.min).AddDays(-beforeStartDayShift)
    let endDate = report |> List.map (fun q -> q.CloseDate) |> List.max
    let currency = report |> List.map (fun q -> q.Currency) |> List.head
    
    let nbpRates =
        getDateRanges startDate endDate 1
        |> Seq.toList
        |> getNbpRates currency
        |> Async.RunSynchronously
        |> List.map JsonConvert.DeserializeObject<NbpExchangeRateDto>
        |> List.collect (fun q -> q.Rates)
    
    generateReport report nbpRates currency
    ()
