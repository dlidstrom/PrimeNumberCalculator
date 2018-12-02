namespace Prime.Core

open Amazon.Lambda.Core
open Amazon.Lambda.Serialization.Json

[<assembly:LambdaSerializer(typeof<JsonSerializer>)>]
do ()

type Request = { Key1 : string; Key2 : string; Key3 : string }
type Response = { Message : string; Request : Request }

module Handler =
    let x = MillerRabin.isPrime 12
    let hello (request : Request) =
        {
            Message = "Go Serverless v1.0! Your function executed successfully!"
            Request = request
        }
