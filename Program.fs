module FileSquid.Program

open System
open FileSquid.Pattern.String

let domain = ArrayDomain ()
let str1 = domain.Expand ()
let str2 = domain.Expand ()
let str3 = domain.Expand ()

let pattern = concat [lit "root/"; var str1; lit "/"; var str2; lit "-"; var str3; lit ".mp3"]
let res1 = pattern.Apply "root/bleh/blurg-blar.mp3" [] (domain.Create ())
let res2 = pattern.Apply "root/ab/cd-ef.mp3" [] (domain.Create ())
let res3 = pattern.Apply "test.text" [] (domain.Create ())
let res4 = pattern.Apply "root/abl/et/fs-se.mp3" [] (domain.Create ())
Console.ReadKey () |> ignore