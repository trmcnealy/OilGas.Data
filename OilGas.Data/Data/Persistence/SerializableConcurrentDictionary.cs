using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Globalization;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;

namespace OilGas.Data
{
    [Serializable]
    public class SerializableConcurrentDictionary<TKey, TValue> : ConcurrentDictionary<TKey, TValue>, ISerializable
    {
        public SerializableConcurrentDictionary()
        {
        }

        public SerializableConcurrentDictionary(int concurrencyLevel,
                                                int capacity)
            : base(concurrencyLevel,
                   capacity)
        {
        }

        public SerializableConcurrentDictionary(IEnumerable<KeyValuePair<TKey, TValue>> collection)
            : base(collection)
        {
        }

        public SerializableConcurrentDictionary(IEqualityComparer<TKey> comparer)
            : base(comparer)
        {
        }

        public SerializableConcurrentDictionary(IEnumerable<KeyValuePair<TKey, TValue>> collection,
                                                IEqualityComparer<TKey>                 comparer)
            : base(collection,
                   comparer)
        {
        }

        public SerializableConcurrentDictionary(int                                     concurrencyLevel,
                                                IEnumerable<KeyValuePair<TKey, TValue>> collection,
                                                IEqualityComparer<TKey>                 comparer)
            : base(concurrencyLevel,
                   collection,
                   comparer)
        {
        }

        public SerializableConcurrentDictionary(IDictionary<TKey, TValue> dictionary)
            : base(dictionary)
        {
        }

        public SerializableConcurrentDictionary(IDictionary<TKey, TValue> dictionary,
                                                IEqualityComparer<TKey>   comparer)
            : base(dictionary,
                   comparer)
        {
        }

        #region ISerializable Members

        protected SerializableConcurrentDictionary(SerializationInfo info,
                                                   StreamingContext  context)
        {
            int itemCount = info.GetInt32("itemsCount");

            for(int i = 0; i < itemCount; i++)
            {
                KeyValuePair<TKey, TValue> kvp = (KeyValuePair<TKey, TValue>)info.GetValue($"Item{i}",
                                                                                           typeof(KeyValuePair<TKey, TValue>));

                TryAdd(kvp.Key,
                       kvp.Value);
            }
        }

        void ISerializable.GetObjectData(SerializationInfo info,
                                         StreamingContext  context)
        {
            info.AddValue("itemsCount",
                          Count);

            int itemIdx = 0;

            foreach(KeyValuePair<TKey, TValue> kvp in this)
            {
                info.AddValue($"Item{itemIdx}",
                              kvp,
                              typeof(KeyValuePair<TKey, TValue>));

                itemIdx++;
            }
        }

        #endregion
    }
}