namespace FileSquid

open System
open System.Collections
open System.Collections.Generic

/// A node for a trie where every node is associated with a value.
type [<AllowNullLiteral>] TrieNode<'a> (children : TrieNode<'a>[], value : 'a) =
    let mutable value = value
    new (alphabetSize, value) = TrieNode<'a> (Array.zeroCreate alphabetSize, value)
    new (source : TrieNode<'a>) = TrieNode<'a> (Array.copy source.Children, source.Value)
    
    /// Gets the children for this node.
    member this.Children = children

    /// Gets or sets the terminal value for this node.
    member this.Value
        with get () = value
        and set nValue = value <- nValue

    /// Gets or sets an indexed child for this node.
    member this.Item
        with get index = children.[index]
        and set index value = children.[index] <- value

/// A mapping of strings (over a given alphabet) to values of certain type.
type Trie<'a, 'b when 'b : equality> (alphabet : Alphabet<'a>, root : TrieNode<'b>, initial : 'b) =
    new (alphabet, initial) = Trie<'a, 'b> (alphabet, TrieNode<'b> (alphabet.Size, initial), initial)

    /// Gets the alphabet for this trie.
    member this.Alphabet = alphabet

    /// Gets the root node for this trie.
    member this.Root = root

    /// Gets the initial value for this trie.
    member this.Initial = initial

    /// Gets or sets the value for a string in this trie.
    member this.Item
        with get (string : seq<'a>) =
            let mutable current = root
            let mutable isNull = false
            let enumerator = string.GetEnumerator ()
            while enumerator.MoveNext () && not isNull do
                current <- current.[alphabet.[enumerator.Current]]
                isNull <- obj.ReferenceEquals (current, null)
            if isNull then initial else current.Value
        and set (string : seq<'a>) value =
            let mutable current = root
            for symbol in string do
                let index = alphabet.[symbol]
                let next = current.[index]
                if obj.ReferenceEquals (next, null) then
                    let next = TrieNode<'b> (alphabet.Size, initial)
                    current.[index] <- next
                    current <- next
            current.Value <- value

    /// Gets the key/value pairs for the non-default values in this trie.
    member this.Items =
        let items = List<KeyValuePair<'a[], 'b>> ()
        let rec writeItems length accum (node : TrieNode<'b>) =
            if node.Value <> initial then
                let string = Array.zeroCreate length
                let mutable i = length
                for symbol in accum do
                    i <- i - 1
                    string.[i] <- symbol
                items.Add (KeyValuePair<'a[], 'b> (string, node.Value))
            for index = 0 to node.Children.Length - 1 do
                let child = node.Children.[index]
                if not (obj.ReferenceEquals (child, null)) then
                    let symbol = alphabet.[index]
                    writeItems (length + 1) (symbol :: accum) child
        writeItems 0 [] root
        items :> seq<KeyValuePair<'a[], 'b>>

    interface IEnumerable<KeyValuePair<'a[], 'b>> with
        member this.GetEnumerator () = this.Items.GetEnumerator ()
        member this.GetEnumerator () = this.Items.GetEnumerator () :> IEnumerator

/// Contains functions for constructing, manipulating and using tries.
module Trie =
    
    /// Creates a trie with the given alphabet and initial value.
    let create (alphabet : Alphabet<'a>) (initial : 'b) = Trie<'a, 'b> (alphabet, initial)

    /// Gets the value of a trie for the given string.
    let get (trie : Trie<'a, 'b>) (string : seq<'a>) = trie.[string]

    /// Sets the value of a trie for the given string.
    let set (trie : Trie<'a, 'b>) (string : seq<'a>) value = trie.[string] <- value

    /// Maps the given trie for all substrings of the given string.
    let mapForSubstrings (map : 'b -> 'b) (string  : seq<'a>) (trie : Trie<'a, 'b>) =
        let alphabet = trie.Alphabet
        let root = TrieNode<'b> (trie.Root.Children, map trie.Root.Value)
        let mapInitial = map trie.Initial
        let mutable currentTerminals = List<TrieNode<'b>> ()
        let mutable nextTerminals = List<TrieNode<'b>> ()
        currentTerminals.Add root

        for symbol in string do
            let index = alphabet.[symbol]

            // Map the current terminal nodes, and prepare the terminal nodes for the next symbol.
            let mutable head = null
            nextTerminals.Clear ()
            for terminal in currentTerminals do
                let next = terminal.[index]
                if obj.ReferenceEquals (next, null) then
                    if obj.ReferenceEquals (head, null) then
                        head <- TrieNode<'b> (alphabet.Size, mapInitial)
                    terminal.[index] <- head
                else 
                    let newNext = TrieNode<'b> (Array.copy next.Children, map next.Value)
                    terminal.[index] <- newNext
                    nextTerminals.Add newNext
            
            // Note that the terminal nodes must be sorted by depth, with the deepest node occuring first.
            if not (obj.ReferenceEquals (head, null)) then nextTerminals.Insert (0, head)
            nextTerminals.Add root
            root.Value <- map root.Value

            let temp = currentTerminals
            currentTerminals <- nextTerminals
            nextTerminals <- temp
        Trie<'a, 'b> (alphabet, root, trie.Initial)

    /// Creates a trie with the value "true" for all substrings of the given string.
    let hasSubstring (alphabet : Alphabet<'a>) (string : seq<'a>) =
        let trie = create alphabet false
        mapForSubstrings (fun _ -> true) string trie

    /// Creates a trie that maps each string to the number of occurences in the given string.
    let countSubstrings (alphabet : Alphabet<'a>) (string : seq<'a>) =
        let trie = create alphabet 0
        mapForSubstrings ((+) 1) string trie