module FileSquid.Program

open System
open FileSquid.Pattern

let domain = ArrayDomain ()
let str1 = domain.Expand ()
let pattern = str1

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

let input = Input.create domain pattern id objs
let tree = SuffixTree.createGeneralized Alphabet.ascii (objs |> Seq.map (fun obj -> (obj, obj :> seq<char>)))
let test1 = SuffixTree.contains tree "square"
let test2 = SuffixTree.contains tree "(Live)"
let test3 = SuffixTree.contains tree ".jpg"
let test4 = SuffixTree.contains tree "I'm In"
let test5 = SuffixTree.contains tree "nope"

Console.ReadKey () |> ignore