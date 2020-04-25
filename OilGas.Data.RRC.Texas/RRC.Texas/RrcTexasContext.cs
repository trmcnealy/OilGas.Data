using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Data;
using System.Diagnostics;
using System.IO;
using System.Runtime.CompilerServices;
using System.Runtime.Serialization;
using System.Threading.Tasks;
using System.Xml;
using System.Xml.Serialization;

using AngleSharp.Html.Dom;

using Microsoft.Data.Sqlite;
using Microsoft.EntityFrameworkCore;

namespace OilGas.Data.RRC.Texas
{
    [XmlRoot("RrcTexasDb")]
    [Serializable]
    [DataContract]
    public sealed class RrcTexasContext : DbContext
    {
        public const string DefaultDbName = "Rrc.Texas.db";
        public const string InMemoryName  = ":memory:";

        public DbSet<DrillingPermit> DrillingPermits { get; set; }

        public DbSet<WellProduction> WellProductions { get; set; }

        public DbSet<WellProductionRecord> WellProductionRecords { get; set; }

        public SqliteConnection Connection { get; }

        private static SqliteConnection CreateAndOpen()
        {
            SqliteConnection connection = new SqliteConnection($"Data Source={InMemoryName};Mode=Memory;Cache=Shared");

            connection.Open();

            return connection;
        }

        public RrcTexasContext()
            : this(CreateAndOpen())
        {
        }

        public RrcTexasContext(SqliteConnection connection)
            : base(new DbContextOptionsBuilder<RrcTexasContext>().UseSqlite(connection).Options)
        {
            Connection = connection;

            Database.EnsureCreated();
        }

        public override void Dispose()
        {
            base.Dispose();

            if(Connection.State == ConnectionState.Open)
            {
                Connection.Close();
            }
        }

        public void Backup()
        {
            SqliteConnection backup = new SqliteConnection($"Data Source={DefaultDbName}");
            Connection.BackupDatabase(backup);
            backup.Close();
        }

        public void LoadDb(string filePath)
        {
            SqliteConnection dbFile = new SqliteConnection($"Data Source={filePath}");
            dbFile.Open();
            dbFile.BackupDatabase(Connection);
            dbFile.Close();
        }

        public void LoadDrillingPermitsCsv(string filePath)
        {
            CsvReader csvReader = new CsvReader(File.ReadAllBytes(filePath));

            (List<string[]> header, List<string[]> rows) = csvReader.ReadFile(1);

            DrillingPermit[] entries = new DrillingPermit[rows.Count];

            Parallel.ForEach(Partitioner.Create(0, rows.Count),
                             (row,
                              loopState) =>
                             {
                                 int index;

                                 string    statusDate;
                                 int?      status;
                                 ApiNumber api;
                                 string    operatorNameNumber;
                                 string    leaseName;
                                 string    well;
                                 string    dist;
                                 string    county;
                                 string    wellboreProfile;
                                 string    filingPurpose;
                                 string    amend;
                                 string    totalDepth;
                                 string    stackedLateralParentWellDp;
                                 string    currentQueue;

                                 //for(int i = 0; i < rows.Count; i++)
                                 for(int i = row.Item1; i < row.Item2; i++)
                                 {
                                     index = 0;

                                     statusDate                 = rows[i][index++];
                                     status                     = int.Parse(rows[i][index++]);
                                     api                        = new ApiNumber("42" + rows[i][index++]);
                                     operatorNameNumber         = rows[i][index++];
                                     leaseName                  = rows[i][index++];
                                     well                       = rows[i][index++];
                                     dist                       = rows[i][index++];
                                     county                     = rows[i][index++];
                                     wellboreProfile            = rows[i][index++];
                                     filingPurpose              = rows[i][index++];
                                     amend                      = rows[i][index++];
                                     totalDepth                 = rows[i][index++];
                                     stackedLateralParentWellDp = rows[i][index++];
                                     currentQueue               = rows[i][index];

                                     entries[i] = new DrillingPermit(statusDate,
                                                                     status,
                                                                     api,
                                                                     operatorNameNumber,
                                                                     leaseName,
                                                                     well,
                                                                     dist,
                                                                     county,
                                                                     wellboreProfile,
                                                                     filingPurpose,
                                                                     amend,
                                                                     totalDepth,
                                                                     stackedLateralParentWellDp,
                                                                     currentQueue);
                                 }
                             });

            using(SqliteTransaction transaction = Connection.BeginTransaction(IsolationLevel.ReadCommitted))
            {
                DrillingPermits.AddRange(entries);

                transaction.Commit();

                SaveChanges();
            }
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public async Task<IEnumerable<Lease>> GetLeaseByApi(ApiNumber api)
        {
            IHtmlDocument htmlDoc = await QueryBuilder.WellboreQueryByApi(api);

            List<WellboreQueryData> wellboreQueriesData = QueryParser.ParseWellboreQuery(htmlDoc);

            List<Lease> leases = new List<Lease>(10);

            foreach(WellboreQueryData wellboreQueryData in wellboreQueriesData)
            {
                LeaseDetailQueryData leaseDetailQueryData = await QueryBuilder.LeaseDetailQuery(wellboreQueryData);

                leases.Add(Lease.Create(wellboreQueryData, leaseDetailQueryData));
            }

            return leases;
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public async Task<WellProduction> GetProductionByApi(ApiNumber api,
                                                             bool      persistentData = true,
                                                             bool      updateData     = true)
        {
            if(persistentData && !updateData)
            {
                try
                {
                    WellProduction record = await WellProductions.Include(wp => wp.Records).FirstOrDefaultAsync(w => w.Api == api);

                    if(record != null)
                    {
                        return record;
                    }
                }
                catch(Exception ex)
                {
                    Debug.WriteLine(ex.Message);
                }
            }

            if(!updateData)
            {
                return null;
            }

            (string csvData, Lease lease) = await QueryBuilder.SpecificLeaseProductionQueryByApi(api);

            CsvReader csvReader = new CsvReader(csvData);

            (List<string[]> header, List<string[]> rows) data = csvReader.ReadFile(10);

            List<SpecificLeaseProductionQueryData> productionData = new List<SpecificLeaseProductionQueryData>(data.rows.Count);

            foreach(string[] entry in data.rows)
            {
                productionData.Add(new SpecificLeaseProductionQueryData(api, entry));
            }

            WellProduction wellProduction = WellProduction.ConvertFrom(lease, productionData);

            if(persistentData)
            {
                WellProduction record = await WellProductions.Include(wp => wp.Records).FirstOrDefaultAsync(w => api.Equals(w.Api));

                if(record != null)
                {
                    record.Records = wellProduction.Records;
                    Update(record);
                }
                else
                {
                    record = wellProduction;
                    await AddAsync(record);
                }

                await SaveChangesAsync();

                return record;
            }

            return wellProduction;
        }

        //public void Save()
        //{
        //    XmlSerializer ser = new XmlSerializer(typeof(RrcTexasContext));

        //    using(FileStream writer = new FileStream(DataStorage.FullPath,
        //                                             FileMode.Create))
        //    {
        //        using(XmlWriter xmlWriter = XmlWriter.Create(writer,
        //                                                     new XmlWriterSettings
        //                                                     {
        //                                                         Indent = false
        //                                                     }))
        //        {
        //            ser.Serialize(xmlWriter,
        //                          this);
        //        }

        //        writer.Flush();
        //    }
        //}

        //public void Load()
        //{
        //    if(!File.Exists(DataStorage.FullPath))
        //    {
        //        return;
        //        //throw new FileNotFoundException(DataStorage.FullPath);
        //    }

        //    using FileStream fs = new FileStream(DataStorage.FullPath,
        //                                         FileMode.Open);

        //    XmlDictionaryReaderQuotas xmlQuotas = new XmlDictionaryReaderQuotas();

        //    XmlDictionaryReader reader = XmlDictionaryReader.CreateTextReader(fs,
        //                                                                      xmlQuotas);

        //    XmlSerializer ser = new XmlSerializer(typeof(RrcTexasContext));

        //    RrcTexasContext db = (RrcTexasContext)ser.Deserialize(reader);

        //    WellProductions = db.WellProductions;

        //    reader.Close();

        //    fs.Close();
        //}

        //public async Task<bool> InsertAsync(WellProduction wellProduction)
        //{
        //    if(wellProduction.Id == 0)
        //    {
        //        wellProduction.Id = WellProductions.Count;
        //    }

        //    return await Task.Run(() => WellProductions.TryAdd(wellProduction.Api,
        //                                                       wellProduction));
        //}

        //public async Task<bool> UpdateAsync(WellProduction newValue)
        //{
        //    if(WellProductions.TryGetValue(newValue.Api,
        //                                   out WellProduction oldValue))
        //    {
        //        newValue.Id = oldValue.Id;

        //        return await Task.Run(() => WellProductions.TryUpdate(newValue.Api,
        //                                                              newValue,
        //                                                              oldValue));
        //    }

        //    return await Task.Run(() => WellProductions.TryAdd(newValue.Api,
        //                                                       newValue));
        //}

        //public async Task<bool> RemoveAsync(WellProduction wellProduction)
        //{
        //    return await Task.Run(() => WellProductions.TryRemove(wellProduction.Api,
        //                                                          out _));
        //}

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
