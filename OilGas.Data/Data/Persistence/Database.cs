using System;
using System.Collections;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.IO;
using System.Runtime.CompilerServices;
using System.Runtime.Serialization;
using System.Threading;
using System.Threading.Tasks;
using System.Xml;
using System.Xml.Serialization;

namespace OilGas.Data
{
    [XmlRoot("Dictionary")]
    [Serializable]
    public class SerializableDictionary<TKey, TValue> : Dictionary<TKey, TValue>, IXmlSerializable
    {
        public SerializableDictionary()
        {
        }

        public SerializableDictionary(int capacity)
            : base(capacity)
        {
        }

        public SerializableDictionary(IEqualityComparer<TKey> comparer)
            : base(comparer)
        {
        }

        public SerializableDictionary(int                     capacity,
                                      IEqualityComparer<TKey> comparer)
            : base(capacity,
                   comparer)
        {
        }

        public SerializableDictionary(IDictionary<TKey, TValue> dictionary)
            : base(dictionary)
        {
        }

        public SerializableDictionary(IDictionary<TKey, TValue> dictionary,
                                      IEqualityComparer<TKey>   comparer)
            : base(dictionary,
                   comparer)
        {
        }

        protected SerializableDictionary(SerializationInfo info,
                                         StreamingContext  context)
            : base(info,
                   context)
        {
        }

        //[SecurityCritical]
        //public virtual void GetObjectData(SerializationInfo info, StreamingContext context);
        //public virtual void OnDeserialization(object sender);

        #region IXmlSerializable Members

        public System.Xml.Schema.XmlSchema GetSchema()
        {
            return null;
        }

        public void ReadXml(XmlReader reader)
        {
            XmlSerializer keySerializer   = new XmlSerializer(typeof(TKey));
            XmlSerializer valueSerializer = new XmlSerializer(typeof(TValue));

            bool wasEmpty = reader.IsEmptyElement;
            reader.Read();

            if(wasEmpty)
            {
                return;
            }

            while(reader.NodeType != XmlNodeType.EndElement)
            {
                reader.ReadStartElement("item");

                reader.ReadStartElement("key");
                TKey key = (TKey)keySerializer.Deserialize(reader);
                reader.ReadEndElement();

                reader.ReadStartElement("value");
                TValue value = (TValue)valueSerializer.Deserialize(reader);
                reader.ReadEndElement();

                Add(key,
                    value);

                reader.ReadEndElement();
                reader.MoveToContent();
            }

            reader.ReadEndElement();
        }

        public void WriteXml(XmlWriter writer)
        {
            XmlSerializer keySerializer   = new XmlSerializer(typeof(TKey));
            XmlSerializer valueSerializer = new XmlSerializer(typeof(TValue));

            writer.WriteStartElement("SerializableDictionaryOf" +
                                     typeof(TKey).Name.Replace("`",
                                                               "") +
                                     typeof(TValue).Name.Replace("`",
                                                                 ""));

            foreach(TKey key in Keys)
            {
                writer.WriteStartElement("item");

                writer.WriteStartElement("key");

                keySerializer.Serialize(writer,
                                        key);

                writer.WriteEndElement();

                writer.WriteStartElement("value");
                TValue value = this[key];

                valueSerializer.Serialize(writer,
                                          value);

                writer.WriteEndElement();

                writer.WriteEndElement();
            }

            writer.WriteEndElement();
        }

        #endregion IXmlSerializable Members
    }

    [XmlRoot("ConcurrentDictionary")]
    [Serializable]
    public class SerializableConcurrentDictionary<TKey, TValue> : ConcurrentDictionary<TKey, TValue>, IXmlSerializable
    {
        public SerializableConcurrentDictionary()
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

        public SerializableConcurrentDictionary(int concurrencyLevel,
                                                int capacity)
            : base(concurrencyLevel,
                   capacity)
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

        public SerializableConcurrentDictionary(int                     concurrencyLevel,
                                                int                     capacity,
                                                IEqualityComparer<TKey> comparer)
            : base(concurrencyLevel,
                   capacity,
                   comparer)
        {
        }
        
        #region IXmlSerializable Members

        public System.Xml.Schema.XmlSchema GetSchema()
        {
            return null;
        }

        public void ReadXml(XmlReader reader)
        {
            XmlSerializer keySerializer   = new XmlSerializer(typeof(TKey));
            XmlSerializer valueSerializer = new XmlSerializer(typeof(TValue));

            bool wasEmpty = reader.IsEmptyElement;
            reader.Read();

            if(wasEmpty)
            {
                return;
            }

            while(reader.NodeType != XmlNodeType.EndElement)
            {
                reader.ReadStartElement("item");

                reader.ReadStartElement("key");
                TKey key = (TKey)keySerializer.Deserialize(reader);
                reader.ReadEndElement();

                reader.ReadStartElement("value");
                TValue value = (TValue)valueSerializer.Deserialize(reader);
                reader.ReadEndElement();

                TryAdd(key,
                       value);

                reader.ReadEndElement();
                reader.MoveToContent();
            }

            //reader.ReadEndElement();
        }

        public void WriteXml(XmlWriter writer)
        {
            XmlSerializer keySerializer   = new XmlSerializer(typeof(TKey));
            XmlSerializer valueSerializer = new XmlSerializer(typeof(TValue));

            writer.WriteAttributeString("Type",
                                        "SerializableConcurrentDictionary");

            writer.WriteAttributeString("TKey",
                                        typeof(TKey).Name);

            writer.WriteAttributeString("TValue",
                                        typeof(TValue).Name);

            foreach(TKey key in Keys)
            {
                writer.WriteStartElement("item");

                writer.WriteStartElement("key");

                keySerializer.Serialize(writer,
                                        key);

                writer.WriteEndElement();

                writer.WriteStartElement("value");
                TValue value = this[key];

                valueSerializer.Serialize(writer,
                                          value);

                writer.WriteEndElement();

                writer.WriteEndElement();
            }
        }

        #endregion IXmlSerializable Members
    }

    [XmlRoot("Database")]
    [Serializable]
    [DataContract]
    public sealed class Database<TKey, TValue>
        where TKey : struct
        where TValue : class
    {
        [DataMember]
        [XmlElement]
        public SerializableConcurrentDictionary<TKey, TValue> KeyValues { get; set; }

        [DataMember]
        [XmlElement]
        public DataStorage DataStorage { get; set; }

        internal Database()
        {
        }

        public Database(DataStorage dataStorage)
        {
            DataStorage = dataStorage;
            KeyValues   = new SerializableConcurrentDictionary<TKey, TValue>();
        }

        internal Database(Database<TKey, TValue> db)
        {
            DataStorage = db.DataStorage;
            KeyValues   = db.KeyValues;
        }

        public void Save()
        {
            XmlSerializer ser = new XmlSerializer(typeof(Database<TKey, TValue>));

            using(FileStream writer = new FileStream(DataStorage.FullPath,
                                                     FileMode.Create))
            {
                using(XmlWriter xmlWriter = XmlWriter.Create(writer,
                                                             new XmlWriterSettings
                                                             {
                                                                 Indent = false
                                                             }))
                {
                    ser.Serialize(xmlWriter,
                                  this);
                }

                writer.Flush();
            }
        }

        public static Database<TKey, TValue> Load(DataStorage dataStorage)
        {
            if(!File.Exists(dataStorage.FullPath))
            {
                return new Database<TKey, TValue>(dataStorage);

            }

            using FileStream fs = new FileStream(dataStorage.FullPath,
                                                 FileMode.Open);

            XmlDictionaryReaderQuotas xmlQuotas = new XmlDictionaryReaderQuotas();

            XmlDictionaryReader reader = XmlDictionaryReader.CreateTextReader(fs,
                                                                              xmlQuotas);

            XmlSerializer ser = new XmlSerializer(typeof(Database<TKey, TValue>));

            Database<TKey, TValue> db = new Database<TKey, TValue>((Database<TKey, TValue>)ser.Deserialize(reader));

            reader.Close();

            fs.Close();

            return db;
        }
    }
}