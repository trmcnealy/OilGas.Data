using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading.Tasks;

using AngleSharp.Html.Dom;

using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.ChangeTracking;
using Microsoft.EntityFrameworkCore.Sqlite;
using Microsoft.Extensions.Caching.Memory;

namespace OilGas.Data.RRC.Texas
{
    internal sealed class RrcTexasContext : DbContext
    {
        //public DbSet<Lease> Leases { get; set; }

        public DbSet<WellProduction> WellProductions { get; set; }

        public DbSet<WellProductionRecord> WellProductionRecords { get; set; }

        public DataStorage DataStorage { get; }

        public RrcTexasContext()
        {
            DataStorage = new DataStorage("Rrc.Texas.db");
        }

        public RrcTexasContext(DataStorage dataStorage)
        {
            DataStorage = dataStorage;
        }

        public RrcTexasContext(DbContextOptions<RrcTexasContext> options)
            : base(options)
        {
        }

        protected override void OnConfiguring(DbContextOptionsBuilder optionsBuilder)
        {
            optionsBuilder.UseMemoryCache(new MemoryCache(new MemoryDistributedCacheOptions()));
            optionsBuilder.UseSqlite($"Data Source={DataStorage.FullPath}");
        }

        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            modelBuilder.Entity<WellProduction>().HasIndex(b => b.Api).IsUnique();

            modelBuilder.Entity<WellProductionRecord>().HasIndex(b => b.Id).IsUnique();
        }
    }

    public sealed class RrcTexasDataAdapter : IDisposable
    {
        private static readonly RrcTexasDataAdapter instance = new RrcTexasDataAdapter();

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        private static RrcTexasDataAdapter GetInstance()
        {
            return instance;
        }

        internal static RrcTexasDataAdapter Instance
        {
#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
            get { return GetInstance(); }
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        static RrcTexasDataAdapter()
        {
            AppDomain.CurrentDomain.ProcessExit += RrcTexasDataAdapter_Dtor;
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        private static void RrcTexasDataAdapter_Dtor(object    sender,
                                                     EventArgs e)
        {
            Instance.Dispose();
        }

        internal RrcTexasContext DbContext
        {
#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
            get;
#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
            private set;
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        private RrcTexasDataAdapter()
        {
            DbContext = new RrcTexasContext();
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void Dispose()
        {
            DbContext.SaveChanges();
            DbContext.Dispose();
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public static void Initialize(DataStorage dbPath = null)
        {
            Instance.DbContext.Database.EnsureCreated();

            if(dbPath == null)
            {
                return;
            }

            if(Instance.DbContext == null)
            {
                Instance.DbContext = new RrcTexasContext(dbPath);
            }
            else if(Instance.DbContext.Database.GetDbConnection().Database != dbPath.FullPath)
            {
                Instance.DbContext.Dispose();

                Instance.DbContext = new RrcTexasContext(dbPath);
            }
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public static async Task<bool> Add(WellProduction wellProduction)
        {
            try
            {
                WellProduction record = await Instance.DbContext.WellProductions.Include(x => x.Records).FirstOrDefaultAsync(x => x.Id == wellProduction.Id);

                EntityEntry<WellProduction> entry;

                if(record == null)
                {
                    entry = await Instance.DbContext.WellProductions.AddAsync(wellProduction);
                }
                else
                {
                    entry = Instance.DbContext.WellProductions.Update(wellProduction);
                }

                if(entry != null)
                {
                    return true;
                }
            }
            catch(Exception ex)
            {
                Debug.WriteLine(ex.Message);
            }

            return false;
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public static async Task<bool> Add(WellProductionRecord wellProductionRecord)
        {
            try
            {
                WellProductionRecord record =
                    await Instance.DbContext.WellProductionRecords.FirstOrDefaultAsync(x => x.WellProduction == wellProductionRecord.WellProduction && x.Month == wellProductionRecord.Month);

                EntityEntry<WellProductionRecord> entry;

                if(record == null)
                {
                    entry = await Instance.DbContext.WellProductionRecords.AddAsync(wellProductionRecord);
                }
                else
                {
                    entry = Instance.DbContext.WellProductionRecords.Update(wellProductionRecord);
                }

                if(entry != null)
                {
                    return true;
                }
            }
            catch(Exception ex)
            {
                Debug.WriteLine(ex.Message);
            }

            return false;
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public static async Task<bool> AddRange(IEnumerable<WellProductionRecord> wellProductionRecords)
        {
            bool result = true;

            foreach(WellProductionRecord record in wellProductionRecords)
            {
                result &= await Add(record);
            }

            return result;
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public static async Task<int> Commit()
        {
            return await Instance.DbContext.SaveChangesAsync();
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public static async Task<IEnumerable<Lease>> GetLeaseByApi(ApiNumber api)
        {
            IHtmlDocument htmlDoc = await QueryBuilder.WellboreQueryByApi(api);

            List<WellboreQueryData> wellboreQueriesData = QueryParser.ParseWellboreQuery(htmlDoc);

            List<Lease> leases = new List<Lease>(10);

            foreach(WellboreQueryData wellboreQueryData in wellboreQueriesData)
            {
                LeaseDetailQueryData leaseDetailQueryData = await QueryBuilder.LeaseDetailQuery(wellboreQueryData);

                leases.Add(Lease.Create(wellboreQueryData,
                                        leaseDetailQueryData));
            }

            return leases;
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public static async Task<WellProduction> GetProductionByApi(ApiNumber api,
                                                                    bool      persistentData = true)
        {
            if(persistentData)
            {
                try
                {
                    WellProduction records = await Instance.DbContext.WellProductions.Include(x => x.Records).FirstOrDefaultAsync(x => x.Api == api.ToString());

                    if(records != null)
                    {
                        return records;
                    }
                }
                catch(Exception ex)
                {
                    Debug.WriteLine(ex.Message);
                }
            }

            string csvData = await QueryBuilder.SpecificLeaseProductionQueryByApi(api);

            CsvReader csvReader = new CsvReader(csvData);

            List<string[]> data = csvReader.ReadFile(10);

            List<SpecificLeaseProductionQueryData> productionData = new List<SpecificLeaseProductionQueryData>(data.Count);

            foreach(string[] entry in data)
            {
                productionData.Add(new SpecificLeaseProductionQueryData(api,
                                                                        entry));
            }

            WellProduction wellProduction = WellProduction.ConvertFrom(productionData);

            if(persistentData)
            {
                await Add(wellProduction);

                await AddRange(wellProduction.Records);
            }

            return wellProduction;
        }
    }
}