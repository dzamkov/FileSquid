namespace FileSquid

open System
open System.Collections.Generic

/// A node for a suffix tree.
type [<AllowNullLiteral>] SuffixTreeNode<'a> (children : SuffixTreeNode<'a>[], terminal : 'a) =
    let mutable terminal = terminal
    new (alphabetSize, terminal) = SuffixTreeNode<'a> (Array.zeroCreate alphabetSize, terminal)
    new (source : SuffixTreeNode<'a>) = SuffixTreeNode<'a> (Array.copy source.Children, source.Terminal)
    
    /// Gets the children for this node.
    member this.Children = children

    /// Gets or sets the terminal value for this node.
    member this.Terminal
        with get () = terminal
        and set value = terminal <- value

    /// Gets or sets an indexed child for this node.
    member this.Item
        with get index = children.[index]
        and set index value = children.[index] <- value

/// A generalized suffix tree for a sequence, or set of sequences of symbols in an alphabet. 
type SuffixTree<'a, 'b> = Alphabet<'a> * SuffixTreeNode<'b>

/// Contains functions for constructing, manipulating and using suffix trees.
module SuffixTree =

    /// Builds a suffix tree on top of the given root node, returning all terminal nodes.
    let internal build (defaultTerminal : 'a) (root : SuffixTreeNode<'a>) (alphabet : Alphabet<'b>) (sequence : seq<'b>) =
        let mutable currentTerminals = List<SuffixTreeNode<'a>> ()
        let mutable nextTerminals = List<SuffixTreeNode<'a>> ()
        currentTerminals.Add root
        for symbol in sequence do
            let index = alphabet.[symbol]
            if index = 76 then 
                ()
            let mutable head = null
            nextTerminals.Clear ()
            for terminal in currentTerminals do
                let next = terminal.[index]
                if obj.ReferenceEquals (next, null) then
                    if obj.ReferenceEquals (head, null) then
                        head <- SuffixTreeNode<'a> (alphabet.Size, defaultTerminal)
                    terminal.[index] <- head
                else 
                    let newNext = SuffixTreeNode<'a> next
                    terminal.[index] <- newNext
                    nextTerminals.Add newNext
            
            // Note: the terminal nodes must be sorted by depth, with the deepest node first.
            if not (obj.ReferenceEquals (head, null)) then nextTerminals.Insert (0, head)
            nextTerminals.Add root

            let temp = currentTerminals
            currentTerminals <- nextTerminals
            nextTerminals <- temp
        currentTerminals

    /// Traverses a suffix tree using the given enumerator, returning false if a non-existant node was reached.
    let rec internal traverse (current : SuffixTreeNode<'a> byref) (alphabet : Alphabet<'b>) (enumerator : IEnumerator<'b>) =
        if not (obj.ReferenceEquals (current, null)) then
            if enumerator.MoveNext () then
                current <- current.[alphabet.[enumerator.Current]]
                traverse &current alphabet enumerator
            else true
        else false

    /// Creates a suffix tree for a sequence of symbols in the given alphabet.
    let create (alphabet : Alphabet<'a>)  (sequence : seq<'a>) =
        let root = SuffixTreeNode<bool> (alphabet.Size, false)
        for terminal in build false root alphabet sequence do terminal.Terminal <- true
        (alphabet, root) : SuffixTree<'a, bool>

    /// Determines whether the given suffix tree contains the given substring.
    let contains (tree : SuffixTree<'a, 'b>) (sequence : seq<'a>) =
        let alphabet, root = tree
        let mutable current = root
        traverse &current alphabet (sequence.GetEnumerator ())

    /// Creates a generalized suffix tree that contains all of the given sequences.
    let createGeneralized (alphabet : Alphabet<'a>) (sequences : seq<'k * seq<'a>>) =
        let root = SuffixTreeNode<'k list> (alphabet.Size, [])
        for (key, sequence) in sequences do
            for terminal in build [] root alphabet sequence do
                terminal.Terminal <- key :: terminal.Terminal
        (alphabet, root) : SuffixTree<'a, 'k list>