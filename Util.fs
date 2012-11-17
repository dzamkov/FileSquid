module FileSquid.Util

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

/// An expandable array that allows any index to be used (including negatives).
type BigArray<'a> (first, length, initial : 'a) =
    let mutable first = first
    let mutable initial = initial
    let mutable data = Array.create length initial

    /// Gets the index of the first item stored in the source array.
    member this.First = first

    /// Gets the source array for this big array.
    member this.Data = data

    /// Gets or sets an item in this array.
    member this.Item
        with get index =
            let index = index - first
            if index < 0 || index >= data.Length then initial
            else data.[index]
        and set index value =
            let index = index - first
            let length = data.Length
            if index < 0 then
                let nData = Array.zeroCreate (length - index)
                Array.fill nData 0 (-index) initial
                Array.blit data 0 nData (-index) (length - index)
                data <- nData
                first <- first + index
            if index >= length then
                let nData = Array.zeroCreate (length + index)
                Array.blit data 0 nData 0 length
                Array.fill nData length index initial
                data <- nData
            data.[index] <- value

    /// Replaces all of the items of this array with the given value.
    member this.Fill value =
        Array.fill data 0 data.Length value
        initial <- value