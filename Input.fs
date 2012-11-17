namespace FileSquid

open System
open System.Collections.Generic

/// A mapping of objects to their corresponding matches on a certain pattern.
type Input<'a, 'b> = Dictionary<'a, 'b list>

/// Contains functions for creating and manipulating inputs.
module Input =

    /// Counts the number of entries in an input.
    let countEntries (input : Input<'a, 'b>) = input.Count

    /// Counts the number of matches in an input.
    let countMatches (input : Input<'a, 'b>) = input.Values |> Seq.sumBy List.length
    
    /// Creates an input by applying a pattern to all objects in a given sequence.
    let create (domain : Domain<'a>) (pattern : Pattern<'a, 'b>) (map : 'c -> 'b) (entries : seq<'c>) =
        let input = Dictionary<'c, 'a list> ()
        for entry in entries do
            let value = map entry
            let matches = pattern.Match value [] domain.Empty
            if not (matches.IsEmpty) then input.Add (entry, matches)
        input : Input<'c, 'a>

    /// Enumerates all matches in the given input.
    let enumerate (input : Input<'c, 'a>) = seq { 
        for kvp in input do
            for m in kvp.Value do
                yield (kvp.Key, m)
        }