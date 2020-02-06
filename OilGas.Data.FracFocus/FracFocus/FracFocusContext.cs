using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.Serialization;
using System.Threading.Tasks;
using System.Xml;

namespace OilGas.Data.FracFocus
{
    [Serializable]
    [DataContract]
    public sealed class FracFocusContext : IDisposable
    {
        internal const string DefaultDbName = "FracFocus";

        [DataMember]
        public HashSet<Registry> Registries { get; private set; } = new HashSet<Registry>();

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
            FileStream writer = new FileStream(DataStorage.FullPath,
                                               FileMode.Create);

            DataContractSerializer ser = new DataContractSerializer(typeof(FracFocusContext));

            ser.WriteObject(writer,
                            this);

            writer.Close();
        }

        public void Load()
        {
            using FileStream fs = new FileStream(DataStorage.FullPath,
                                                 FileMode.Open);

            XmlDictionaryReaderQuotas xmlQuotas = new XmlDictionaryReaderQuotas();

            XmlDictionaryReader reader = XmlDictionaryReader.CreateTextReader(fs,
                                                                              xmlQuotas);

            DataContractSerializer ser = new DataContractSerializer(typeof(FracFocusContext));

            Registries = ((FracFocusContext)ser.ReadObject(reader,
                                                           true)).Registries;

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

            return await Task.Run(() => Registries.Add(registry));
        }

        public async Task<bool> UpdateAsync(Registry oldValue, Registry newValue)
        {
            newValue.Id = oldValue.Id;

            Registries.Remove(oldValue);

            return await Task.Run(() => Registries.Add(newValue));
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