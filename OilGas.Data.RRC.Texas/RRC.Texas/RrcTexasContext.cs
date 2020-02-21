using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.Serialization;
using System.Threading.Tasks;
using System.Xml;
using System.Xml.Serialization;

namespace OilGas.Data.RRC.Texas
{
    [XmlRoot("RrcTexasDb")]
    [Serializable]
    [DataContract]
    public sealed class RrcTexasContext : IDisposable
    {
        internal const string DefaultDbName = "Rrc.Texas";

        [DataMember]
        [XmlElement]
        public SerializableConcurrentDictionary<string /*API*/, WellProduction> WellProductions { get; set; } =
            new SerializableConcurrentDictionary<string /*API*/, WellProduction>();
            
        [DataMember]
        [XmlElement]
        public DataStorage DataStorage { get; set; }

        public RrcTexasContext()
            : this(new DataStorage(DefaultDbName))
        {
        }

        public RrcTexasContext(DataStorage dataStorage)
        {
            DataStorage = dataStorage;
        }


        internal RrcTexasContext(RrcTexasContext db)
        {
            DataStorage = db.DataStorage;
            WellProductions = db.WellProductions;
        }

        //public void Save()
        //{
        //    FileStream writer = new FileStream(DataStorage.FullPath,
        //                                       FileMode.Create);
        //
        //    DataContractSerializer ser = new DataContractSerializer(typeof(RrcTexasContext));
        //
        //    ser.WriteObject(writer,
        //                    this);
        //
        //    writer.Close();
        //}
        
        public void Save()
        {
            XmlSerializer ser = new XmlSerializer(typeof(RrcTexasContext));

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

            XmlSerializer ser = new XmlSerializer(typeof(RrcTexasContext));

            RrcTexasContext db = (RrcTexasContext)ser.Deserialize(reader);

            WellProductions = db.WellProductions;

            reader.Close();

            fs.Close();
        }

        public void Dispose()
        {
            Save();
            WellProductions.Clear();
        }

        public async Task<bool> InsertAsync(WellProduction wellProduction)
        {
            if(wellProduction.Id == 0)
            {
                wellProduction.Id = WellProductions.Count;
            }

            return await Task.Run(() => WellProductions.TryAdd(wellProduction.Api,
                                                               wellProduction));
        }

        public async Task<bool> UpdateAsync(WellProduction newValue)
        {
            if(WellProductions.TryGetValue(newValue.Api,
                                           out WellProduction oldValue))
            {
                newValue.Id = oldValue.Id;

                return await Task.Run(() => WellProductions.TryUpdate(newValue.Api,
                                                                      newValue,
                                                                      oldValue));
            }

            return await Task.Run(() => WellProductions.TryAdd(newValue.Api,
                                                               newValue));
        }

        public async Task<bool> RemoveAsync(WellProduction wellProduction)
        {
            return await Task.Run(() => WellProductions.TryRemove(wellProduction.Api,
                                                                  out _));
        }

        //public RrcTexasContext(string configurationString)
        //{
        //    DataStorage = new DataStorage(new SQLiteConnectionStringBuilder(configurationString));

        //    IfDatabaseMissingCreate();
        //}

        //public RrcTexasContext(SQLiteConnectionStringBuilder builder)
        //    : base(GetDataProvider(),
        //           builder.ConnectionString)
        //{
        //    DataStorage = new DataStorage(builder);

        //    IfDatabaseMissingCreate();
        //}

        //private void IfDatabaseMissingCreate()
        //{
        //    if(!File.Exists(DataStorage.FullPath))
        //    {
        //        ((SQLiteDataProvider)DataProvider).CreateDatabase(DataStorage.FullPath,
        //                                                          false);

        //        this.CreateTable<WellProduction>();
        //        this.CreateTable<WellProductionRecord>();
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

        //private static IDbConnection GetConnection()
        //{
        //    var dbConnection = new SqlConnection(@"Server=.\SQL;Database=Northwind;Trusted_Connection=True;Enlist=False;");

        //    return new StackExchange.Profiling.Data.ProfiledDbConnection(dbConnection, MiniProfiler.Current);
        //}

        //protected override void OnConfiguring(DbContextOptionsBuilder optionsBuilder)
        //{
        //    optionsBuilder.UseMemoryCache(new MemoryCache(new MemoryDistributedCacheOptions()));
        //    optionsBuilder.UseSqlite($"Data Source={DataStorage.FullPath}");
        //}

        //protected override void OnModelCreating(ModelBuilder modelBuilder)
        //{
        //    modelBuilder.Entity<WellProduction>().HasIndex(b => b.Api).IsUnique();

        //    modelBuilder.Entity<WellProductionRecord>().HasIndex(b => b.Id).IsUnique();
        //}
    }
}