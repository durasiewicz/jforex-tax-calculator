open System
open System.Net.Http
open System.Threading.Tasks
open Newtonsoft.Json

[<Literal>]
let nbpApiDayRangeLimit = 367

[<Literal>]
let beforeStartDayShift = 7

type NbpExchangeRate =
    { No: string
      EffectiveDate: DateTime
      Mid: decimal }
    
type NbpExchangeRateDto =
     { Table: string
       Currency: string
       Code: string
       Rates: List<NbpExchangeRate> }

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

let ranges =
    getDateRanges (new DateTime(2022, 1, 1)) (new DateTime(2023, 5, 1)) 1
    |> Seq.toList
    |> getNbpRates "EUR"
    |> Async.RunSynchronously
    |> List.map JsonConvert.DeserializeObject<NbpExchangeRateDto>
    |> List.collect (fun q -> q.Rates)

let x = ranges

printfn "Hello from F#"
