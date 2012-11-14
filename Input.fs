namespace FileSquid

open System
open System.Collections.Generic

/// A mapping of objects to their corresponding matches on a certain pattern.
type Input<'a, 'b> = Dictionary<'a, 'b list>

/// Contains functions for creating and manipulating inputs.
module Input =
    
    /// Creates an input by applying a pattern to all objects in a given sequence.
    let create (domain : Domain<'a>) (pattern : Pattern<'a, 'b>) (map : 'c -> 'b) (objs : seq<'c>) =
        let input = Dictionary<'c, 'a list> ()
        for obj in objs do
            let value = map obj
            let matches = pattern.Match value [] domain.Empty
            if not (matches.IsEmpty) then input.Add (obj, matches)
        input : Input<'c, 'a>