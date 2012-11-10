using System;
using System.Collections.Generic;
using System.Text;

namespace FileSquid
{
    /// <summary>
    /// Describes a general form of an object using placeholder variables.
    /// </summary>
    /// <typeparam name="K">An identifier for a variable.</typeparam>
    /// <typeparam name="T">The concrete object type this pattern is for (e.g. string).</typeparam>
    /// <typeparam name="M">The variable mapping type used by this pattern.</typeparam>
    public abstract class Pattern<K, T, M>
        where M : IMap<K, object, M>
    {
        /// <summary>
        /// Creates a variable pattern.
        /// </summary>
        public static Pattern<K, T, M> Variable(K Identifier)
        {
            return new Variable<K, T, M>(Identifier);
        }

        /// <summary>
        /// Creates a literal pattern.
        /// </summary>
        public static Pattern<K, T, M> Literal(T Value)
        {
            return new Literal<K, T, M>(Value);
        }

        /// <summary>
        /// Tries evaluating this pattern for the given variable values.
        /// </summary>
        public abstract bool Evaluate(M Variables, out T Value);

        /// <summary>
        /// Finds mappings of variables or values that would make this pattern equivalent to the given
        /// value.
        /// </summary>
        public abstract IEnumerable<M> Match(M Initial, T Value);

        /// <summary>
        /// Substitutes the given variable with the given pattern. Returns null if the type of the variable
        /// does not correspond with the type of the target pattern.
        /// </summary>
        public abstract Pattern<K, T, M> Substitute<NT>(K Variable, Pattern<K, NT, M> Pattern);

        /// <summary>
        /// Gets the domain (referenced variables along with their associated type) for this pattern.
        /// </summary>
        public Dictionary<K, Type> Domain
        {
            get
            {
                Dictionary<K, Type> domain = new Dictionary<K, Type>();
                this.WriteDomain(domain);
                return domain;
            }
        }

        /// <summary>
        /// Writes the domain of this pattern to the given set.
        /// </summary>
        public abstract void WriteDomain(Dictionary<K, Type> Domain);
    }

    /// <summary>
    /// Contains useful functions for creating and manipulating patterns.
    /// </summary>
    public static class Pattern
    {
        /// <summary>
        /// Concatenates the given string patterns.
        /// </summary>
        public static Pattern<K, string, M> Concat<K, M>(IEnumerable<Pattern<K, string, M>> Parts)
            where M : IMap<K, object, M>
        {
            return ConcatPattern<K, M>.Create(Parts);
        }
    }

    /// <summary>
    /// A pattern for an arbitrary variable.
    /// </summary>
    public sealed class Variable<K, T, M> : Pattern<K, T, M>
        where M : IMap<K, object, M>
    {
        public Variable(K Identifier)
        {
            this.Identifier = Identifier;
        }

        /// <summary>
        /// The identifier for this variable pattern.
        /// </summary>
        public readonly K Identifier;

        public override bool Evaluate(M Variables, out T Value)
        {
            object lvalue = Variables[this.Identifier];
            if (lvalue != null && lvalue is T)
            {
                Value = (T)lvalue;
                return true;
            }
            Value = default(T);
            return false;
        }

        public override IEnumerable<M> Match(M Initial, T Value)
        {
            object cur = Initial[this.Identifier];
            if (cur == null)
            {
                Initial[this.Identifier] = Value;
                return new M[] { Initial };
            }
            if (Value.Equals(cur))
            {
                return new M[] { Initial };
            }
            return new M[0];
        }

        public override Pattern<K, T, M> Substitute<NT>(K Variable, Pattern<K, NT, M> Pattern)
        {
            if (this.Identifier.Equals(Variable))
                return Pattern as Pattern<K, T, M>;
            else
                return this;
        }

        public override void WriteDomain(Dictionary<K, Type> Domain)
        {
            Domain[this.Identifier] = typeof(T);
        }
    }

    /// <summary>
    /// A pattern for a specific value.
    /// </summary>
    public sealed class Literal<K, T, M> : Pattern<K, T, M>
        where M : IMap<K, object, M>
    {
        public Literal(T Value)
        {
            this.Value = Value;
        }

        /// <summary>
        /// The value of this literal.
        /// </summary>
        public readonly T Value;

        public override bool Evaluate(M Variables, out T Value)
        {
            Value = this.Value;
            return true;
        }

        public override IEnumerable<M> Match(M Initial, T Value)
        {
            if (Value.Equals(this.Value))
                return new M[] { Initial };
            else
                return new M[0];
        }

        public override Pattern<K, T, M> Substitute<NT>(K Variable, Pattern<K, NT, M> Pattern)
        {
            return this;
        }

        public override void WriteDomain(Dictionary<K, Type> Domain)
        {

        }
    }

    /// <summary>
    /// A string pattern created by concatenating sub-patterns.
    /// </summary>
    public sealed class ConcatPattern<K, M> : Pattern<K, string, M>
        where M : IMap<K, object, M>
    {
        private ConcatPattern(Pattern<K, string, M>[] Parts)
        {
            this.Parts = Parts;
        }

        /// <summary>
        /// Creates a pattern by concatenating the given parts.
        /// </summary>
        public static Pattern<K, string, M> Create(IEnumerable<Pattern<K, string, M>> Parts)
        {
            List<Pattern<K, string, M>> nParts = new List<Pattern<K, string, M>>();
            string literalString = "";
            foreach (Pattern<K, string, M> part in Parts)
            {
                Literal<K, string, M> literal = part as Literal<K, string, M>;
                if (literal != null) 
                    literalString += literal.Value;
                else
                {
                    if (literalString != "")
                    {
                        nParts.Add(new Literal<K, string, M>(literalString));
                        literalString = "";
                    }
                    nParts.Add(part);
                }
            }
            if (nParts.Count == 0)
                return new Literal<K, string, M>(literalString);
            else
            {
                if (literalString != "")
                {
                    nParts.Add(new Literal<K, string, M>(literalString));
                    return new ConcatPattern<K, M>(nParts.ToArray());
                }
                else if (nParts.Count == 1) return nParts[0];
                else return new ConcatPattern<K, M>(nParts.ToArray());
            }
        }

        /// <summary>
        /// The parts of this pattern.
        /// </summary>
        public readonly Pattern<K, string, M>[] Parts;

        public override bool Evaluate(M Variables, out string Value)
        {
            Value = "";
            StringBuilder builder = new StringBuilder();
            for (int t = 0; t < this.Parts.Length; t++)
            {
                string str;
                if (!this.Parts[t].Evaluate(Variables, out str))
                    return false;
                builder.Append(str);
            }
            Value = builder.ToString();
            return true;
        }

        /// <summary>
        /// Determines whether two substrings are the same (using culture-insensitive equality checking).
        /// </summary>
        private static bool _SubstringEqual(string A, int AIndex, string B, int BIndex, int Length)
        {
            while (Length > 0)
            {
                if (A[AIndex] != B[BIndex]) return false;
                AIndex++;
                BIndex++;
                Length--;
            }
            return true;
        }

        /// <summary>
        /// Finds the next occurence of the given string at the given index (using culture-insensitive equality checking),
        /// returning -1 if it does not occur.
        /// </summary>
        private static int _NextOccurence(string Target, string String, int Index)
        {
            while (Index < String.Length - Target.Length)
            {
                for (int t = 0; t < Target.Length; t++)
                {
                    if (Target[t] != String[t + Index])
                        goto nomatch;
                }
                return Index;
            nomatch:
                Index++;
            }
            return -1;
        }

        /// <summary>
        /// An intermediate match result that takes into account matches of the leftmost
        /// parts of a concatenated pattern.
        /// </summary>
        private struct _PossibleMatch
        {
            /// <summary>
            /// The variable map for this match.
            /// </summary>
            public M Map;

            /// <summary>
            /// The index in the input string this possible match is on.
            /// </summary>
            public int Index;
        }

        public override IEnumerable<M> Match(M Initial, string Value)
        {
            int length = Value.Length;
            int partCount = this.Parts.Length;
            List<_PossibleMatch> possibleMatches = new List<_PossibleMatch>();

            // Match the prefix, if one exists.
            int nextPartIndex;
            Pattern<K, string, M> first = this.Parts[0];
            Literal<K, string, M> literal = first as Literal<K, string, M>;
            if (literal != null)
            {
                if (literal.Value.Length <= length && _SubstringEqual(literal.Value, 0, Value, 0, literal.Value.Length))
                {
                    first = this.Parts[1];
                    nextPartIndex = 2;
                    possibleMatches.Add(new _PossibleMatch { Map = Initial, Index = literal.Value.Length });
                }
                else
                {
                    return new M[0];
                }
            }
            else
            {
                nextPartIndex = 1;
                possibleMatches.Add(new _PossibleMatch { Map = Initial, Index = 0 });
            }

            // Match the suffix, if one exists.
            literal = this.Parts[partCount - 1] as Literal<K, string, M>;
            if (literal != null)
            {
                if (literal.Value.Length <= length && _SubstringEqual(literal.Value, 0, Value, length - literal.Value.Length, literal.Value.Length))
                {
                    length -= literal.Value.Length;
                    partCount--;
                }
                else
                {
                    return new M[0];
                }
            }


            // Match middle parts (after the prefix before the final part).
            Pattern<K, string, M> current = first;
            List<_PossibleMatch> nextMatches = new List<_PossibleMatch>();
            while (nextPartIndex < partCount)
            {
                if (possibleMatches.Count == 0) return new M[0];

                Pattern<K, string, M> next = this.Parts[nextPartIndex];
                if ((literal = next as Literal<K, string, M>) != null)
                {
                    foreach (_PossibleMatch p in possibleMatches)
                        for (int lstart = _NextOccurence(literal.Value, Value, p.Index); lstart != -1;
                            lstart = _NextOccurence(literal.Value, Value, lstart + 1))
                        {

                            foreach (M m in current.Match(p.Map.Copy(), Value.Substring(p.Index, lstart - p.Index)))
                                nextMatches.Add(new _PossibleMatch { Map = m, Index = lstart + literal.Value.Length });
                        }
                    nextPartIndex++;
                    next = this.Parts[nextPartIndex];
                }
                else
                {
                    foreach (_PossibleMatch p in possibleMatches)
                        for (int pend = p.Index; pend <= Value.Length; pend++)
                            foreach (M m in current.Match(p.Map.Copy (), Value.Substring(p.Index, pend - p.Index)))
                                nextMatches.Add(new _PossibleMatch { Map = m, Index = pend });
                }

                List<_PossibleMatch> temp = possibleMatches;
                possibleMatches = nextMatches;
                nextMatches = temp;
                nextMatches.Clear();
                current = next;
                nextPartIndex++;
            }

            // Perform final matches.
            List<M> results = new List<M>();
            foreach (_PossibleMatch p in possibleMatches)
                results.AddRange(current.Match(p.Map, Value.Substring(p.Index, length - p.Index)));
            return results;
        }

        public override Pattern<K, string, M> Substitute<NT>(K Variable, Pattern<K, NT, M> Pattern)
        {
            Pattern<K, string, M>[] nParts = new Pattern<K, string, M>[this.Parts.Length];
            for (int t = 0; t < nParts.Length; t++)
                nParts[t] = this.Parts[t].Substitute(Variable, Pattern);
            return Create(nParts);
        }

        public override void WriteDomain(Dictionary<K, Type> Domain)
        {
            foreach (Pattern<K, string, M> part in this.Parts)
                part.WriteDomain(Domain);
        }
    }
}
