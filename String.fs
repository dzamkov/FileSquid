module FileSquid.Extensions

open System

/// Extends the string module with some useful functions.
module String =
    
    /// Determines whether two substrings are the same.
    let rec substringEquals (a : string) aIndex (b : string) bIndex length =
        if length > 0 then a.[aIndex] = b.[bIndex] && substringEquals a (aIndex + 1) b (bIndex + 1) (length - 1)
        else true

    /// Gets the substring between the given indices.
    let between (str : string) l r = str.Substring (l, r - l)

    /// Returns a list of indices where the given target string occurs in the rightmost (at or after index) portion 
    /// of the source string.
    let findOccurences (source : string) (target : string) startAt =
        let mutable results = []
        for tIndex = source.Length - target.Length downto startAt do
            if substringEquals target 0 source tIndex target.Length then
                results <- tIndex :: results
        results