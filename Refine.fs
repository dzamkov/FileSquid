namespace FileSquid

open System
open System.Collections.Generic
open FileSquid.Util

/// Contains functions for refining patterns.
module Refine =

    /// Contains functions for making a pattern better fit all entries in
    /// an input.
    module Fit =
        begin
        end

    /// Contains functions for making a pattern more restrictive in order to
    /// filter a subset of entries in an input.
    module Filter =

        /// Creates an unordered mapping of characters to the number of entries in the 
        /// given input that contain that character.
        let countChars (var : Variable<'a, string>) (input : Input<'b, 'a>) =
            let first = int ' '
            let length = int '~' - int ' '
            let charCounts = BigArray<int> (first, length, 0)
            let hasChar = BigArray<bool> (first, length, false)
            for kvp in input do
                hasChar.Fill false
                for m in kvp.Value do
                    match var.Evaluate m with
                    | Some str ->
                        for ch in str do
                            hasChar.[int ch] <- true
                    | None -> ()
                for i = 0 to hasChar.Data.Length - 1 do
                    if hasChar.Data.[i] then
                        let index = hasChar.First + i
                        charCounts.[index] <- charCounts.[index] + 1
            seq {
                for i = 0 to charCounts.Data.Length - 1 do
                    let count = charCounts.Data.[i]
                    if count > 0 then yield (char (charCounts.First + i), count)
            }