using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.Serialization;
using System.Threading.Tasks;
using System.Xml;

namespace OilGas.Data.RRC.Texas
{
    [Serializable]
    [DataContract]
    public sealed class RrcTexasContext : IDisposable
    {
        internal const string DefaultDbName = "Rrc.Texas";

        [DataMember]
        public HashSet<WellProduction> WellProductions { get; private set; } = new HashSet<WellProduction>();

        [IgnoreDataMember]
        public DataStorage DataStorage { get; }

        public RrcTexasContext()
            : this(new DataStorage(DefaultDbName))
        {
        }

        public RrcTexasContext(DataStorage dataStorage)
        {
            DataStorage = dataStorage;
        }

        public void Save()
        {
            FileStream writer = new FileStream(DataStorage.FullPath,
                                               FileMode.Create);

            DataContractSerializer ser = new DataContractSerializer(typeof(RrcTexasContext));

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

            DataContractSerializer ser = new DataContractSerializer(typeof(RrcTexasContext));

            WellProductions = ((RrcTexasContext)ser.ReadObject(reader,
                                                               true)).WellProductions;

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

            return await Task.Run(() => WellProductions.Add(wellProduction));
        }

        public async Task<bool> UpdateAsync(WellProduction oldValue, WellProduction newValue)
        {
            newValue.Id = oldValue.Id;

            WellProductions.Remove(oldValue);

            return await Task.Run(() => WellProductions.Add(newValue));
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