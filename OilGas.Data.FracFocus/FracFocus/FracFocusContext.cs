using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.Serialization;
using System.Threading.Tasks;
using System.Xml;
using System.Xml.Serialization;

namespace OilGas.Data.FracFocus
{
    [XmlRoot("FracFocusDb")]
    [Serializable]
    [DataContract]
    public sealed class FracFocusContext : IDisposable
    {
        internal const string DefaultDbName = "FracFocus";

        [DataMember]
        [XmlElement]
        public SerializableConcurrentDictionary<string /*API*/, Registry> Registries { get; private set; } = new SerializableConcurrentDictionary<string /*API*/, Registry>();
        
        [DataMember]
        [XmlElement]
        public DataStorage DataStorage { get; }

        public FracFocusContext()
            : this(new DataStorage(DefaultDbName))
        {
        }

        public FracFocusContext(DataStorage dataStorage)
        {
            DataStorage = dataStorage;
        }

        public void Save()
        {
            XmlSerializer ser = new XmlSerializer(typeof(FracFocusContext));

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

        public void Load()
        {
            if(!File.Exists(DataStorage.FullPath))
            {
                return;
                //throw new FileNotFoundException(DataStorage.FullPath);
            }

            using FileStream fs = new FileStream(DataStorage.FullPath,
                                                 FileMode.Open);

            XmlDictionaryReaderQuotas xmlQuotas = new XmlDictionaryReaderQuotas();

            XmlDictionaryReader reader = XmlDictionaryReader.CreateTextReader(fs,
                                                                              xmlQuotas);

            XmlSerializer ser = new XmlSerializer(typeof(FracFocusContext));

            FracFocusContext db = (FracFocusContext)ser.Deserialize(reader);

            Registries = db.Registries;

            reader.Close();

            fs.Close();
        }

        public void Dispose()
        {
            Save();
            Registries.Clear();
        }

        public async Task<bool> InsertAsync(Registry registry)
        {
            if(registry.Id == Guid.Empty)
            {
                registry.Id = Guid.NewGuid();
            }

            return await Task.Run(() => Registries.TryAdd(registry.ApiNumber,
                                                          registry));
        }

        public async Task<bool> UpdateAsync(Registry newValue)
        {
            if(Registries.TryGetValue(newValue.ApiNumber,
                                      out Registry oldValue))
            {
                newValue.Id = oldValue.Id;

                return await Task.Run(() => Registries.TryUpdate(newValue.ApiNumber,
                                                                 newValue,
                                                                 oldValue));
            }

            return await Task.Run(() => Registries.TryAdd(newValue.ApiNumber,
                                                          newValue));
        }

        public async Task<bool> RemoveAsync(Registry registry)
        {
            return await Task.Run(() => Registries.TryRemove(registry.ApiNumber,
                                                             out _));
        }

        //public FracFocusContext(string configurationString)
        //    : base(GetDataProvider(),
        //           configurationString)
        //{
        //    DataStorage = new DataStorage(new SQLiteConnectionStringBuilder(configurationString));

        //    IfDatabaseMissingCreate();
        //}

        //public FracFocusContext(SQLiteConnectionStringBuilder builder)
        //    : base(GetDataProvider(),
        //           builder.ConnectionString)
        //{
        //    DataStorage = new DataStorage(builder);

        //    IfDatabaseMissingCreate();
        //}

        //private void IfDatabaseMissingCreate()
        //{
        //    if(!File.Exists(DataStorage.FullPath) && !IsReadonly)
        //    {
        //        ((SQLiteDataProvider)DataProvider).CreateDatabase(DataStorage.FullPath,
        //                                                          false);

        //        this.CreateTable<Registry>();
        //    }
        //}

        //private static IDataProvider GetDataProvider()
        //{
        //    LinqToDB.Common.Configuration.ContinueOnCapturedContext    = true;
        //    LinqToDB.Common.Configuration.AvoidSpecificDataProviderAPI = true;
        //    LinqToDB.Common.Configuration.Linq.AllowMultipleQuery      = true;
        //    //LinqToDB.Common.Configuration.Linq.DisableQueryCache = true;

        //    return new SQLiteDataProvider(ProviderName.SQLiteClassic);
        //}

        //protected override void OnModelCreating(ModelBuilder modelBuilder)
        //{
        //    modelBuilder.Entity<Registry>();

        //    //modelBuilder.Conventions.Remove<OneToManyCascadeDeleteConvention>();

        //    //modelBuilder.RegisterEntityType(typeof(UnitMeasure));

        //    //modelBuilder.RegisterEntityType(typeof(UnitSystem));
        //}

        //protected override void OnConfiguring(DbContextOptionsBuilder optionsBuilder)
        //{
        //    optionsBuilder.UseMemoryCache(new MemoryCache(new MemoryDistributedCacheOptions()));
        //    optionsBuilder.UseSqlite($@"Data Source={DataStorage.FullPath}");
        //}
    }
}