module FileSquid.Program

open System
open FileSquid.Pattern
open FileSquid.Pattern.String

let domain = ArrayDomain ()
let str1 = domain.Expand ()
let str2 = domain.Expand ()

let pattern = concat [var str1; lit "."; var str2; lit ".mp3"]

let objs = [ "01.Take Five.mp3"
             "02.I'm In A Dancing Mood.mp3"
             "03.In Your Own Sweet Way.mp3"
             "04.Camptown Races.mp3"
             "05.The Duke (Live).mp3"
             "06.It's A Raggy Waltz.mp3"
             "07.Bossa Nova U. S. A..mp3"
             "08.Trolley Song.mp3"
             "09.Unsquare Dance.mp3"
             "10.Blue Rondo A La Turk.mp3"
             "11.Theme From Mr. Broadway.mp3"
             "AlbumArtSmall.jpg"
             "Folder.jpg"
             "TextFileThatIsntSupposedToBeThere.txt" ]

let input = Input.create domain (String.apply pattern) id objs

Console.ReadKey () |> ignore