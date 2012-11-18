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
let mutable trie = Trie<char, int> (Alphabet.ascii, 0)
do for string in objs do trie <- Trie.mapForSubstrings ((+) 1) string trie
let items = trie |> Seq.map (fun kvp -> String kvp.Key, kvp.Value) |> Seq.sortBy (fun (_, n) -> -n) |> Seq.toArray
let test1 = trie.["(Live)"]
let test2 = trie.[""]
let test3 = trie.["Ar"]
let test4 = trie.["ce"]
let test5 = trie.["bleh"]
let test6 = trie.["."]
let test7 = trie.[".mp3"]

Console.ReadKey () |> ignore