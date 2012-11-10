using System;
using System.Collections.Generic;

namespace FileSquid
{
    /// <summary>
    /// A mutable mapping from keys to values where every key in a certain
    /// domain has a value.
    /// </summary>
    public interface IMap<K, V, M>
        where M : IMap<K, V, M>
    {
        /// <summary>
        /// Gets or sets the value for the given key.
        /// </summary>
        V this[K Key] { get; set; }

        /// <summary>
        /// Creates a copy of this map in its current state.
        /// </summary>
        M Copy();
    }

    /// <summary>
    /// A mapping of integers to values implemented with an array.
    /// </summary>
    public struct ArrayMap<V> : IMap<int, V, ArrayMap<V>>
    {
        public ArrayMap(int Size)
            : this(new V[Size])
        {

        }

        public ArrayMap(V[] Source)
        {
            this.Source = Source;
        }

        /// <summary>
        /// The source array for this map.
        /// </summary>
        public V[] Source;

        public V this[int Key]
        {
            get
            {
                return this.Source[Key];
            }
            set
            {
                this.Source[Key] = value;
            }
        }

        public ArrayMap<V> Copy()
        {
            V[] copy = new V[this.Source.Length];
            for (int t = 0; t < copy.Length; t++) copy[t] = this.Source[t];
            return new ArrayMap<V>(copy);
        }
    }

    /// <summary>
    /// A mapping implemented with a dictionary.
    /// </summary>
    public struct DictionaryMap<K, V> : IMap<K, V, DictionaryMap<K, V>>
    {
        public DictionaryMap(Dictionary<K, V> Source)
        {
            this.Source = Source;
        }

        /// <summary>
        /// Creates an empty dictionary map.
        /// </summary>
        public static DictionaryMap<K, V> Create()
        {
            return new DictionaryMap<K, V>(new Dictionary<K, V>());
        }

        /// <summary>
        /// The source dictionary for this map.
        /// </summary>
        public Dictionary<K, V> Source;

        public V this[K Key]
        {
            get
            {
                V result;
                if (this.Source.TryGetValue(Key, out result)) 
                    return result;
                else 
                    return default(V);
            }
            set
            {
                this.Source[Key] = value;
            }
        }

        public DictionaryMap<K, V> Copy()
        {
            return new DictionaryMap<K, V>(new Dictionary<K, V>(this.Source));
        }
    }
}
