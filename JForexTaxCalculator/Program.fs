open System

[<Literal>]
let nbpApiDayRangeLimit = 367

[<Literal>]
let beforeStartDayShift = 7

type NbpExchangeRate =
    { No: string
      EffectiveDate: DateTime
      Mid: decimal }
    
let rec getDateRanges (startDate : DateTime) (endDate : DateTime) (dayShift : int) =
    seq {
        if startDate.AddDays(dayShift) >= endDate then
            yield (startDate, endDate)
        elif dayShift % nbpApiDayRangeLimit = 0 then 
            yield (startDate, startDate.AddDays(dayShift))
            yield! getDateRanges (startDate.AddDays(float(dayShift + 1))) endDate 1
        else
            yield! getDateRanges startDate endDate (dayShift + 1)
    }

let ranges = getDateRanges (new DateTime(2022, 1, 1)) (new DateTime(2023,5,1)) 1
let x = ranges |> Seq.toList

printfn "Hello from F#"