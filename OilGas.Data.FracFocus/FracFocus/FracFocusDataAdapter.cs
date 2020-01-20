using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading.Tasks;

using AngleSharp.Html.Dom;

using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.ChangeTracking;
using Microsoft.EntityFrameworkCore.Sqlite;
using Microsoft.Extensions.Caching.Memory;

namespace OilGas.Data.FracFocus
{
    public sealed class FracFocusContext : DbContext
    {
        public DbSet<Registry> Registries { get; set; }

        public DataStorage DataStorage { get; }

        public FracFocusContext()
        {
            DataStorage = new DataStorage("FracFocus.db");
        }

        public FracFocusContext(DataStorage dataStorage)
        {
            DataStorage = dataStorage;
        }

        public FracFocusContext(DbContextOptions<FracFocusContext> options)
            : base(options)
        {
        }

        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            modelBuilder.Entity<Registry>();

            //modelBuilder.Conventions.Remove<OneToManyCascadeDeleteConvention>();

            //modelBuilder.RegisterEntityType(typeof(UnitMeasure));

            //modelBuilder.RegisterEntityType(typeof(UnitSystem));
        }

        protected override void OnConfiguring(DbContextOptionsBuilder optionsBuilder)
        {
            optionsBuilder.UseMemoryCache(new MemoryCache(new MemoryDistributedCacheOptions()));
            optionsBuilder.UseSqlite($@"Data Source={DataStorage.FullPath}");
        }
    }

    public sealed class FracFocusDataAdapter : IDisposable
    {
        private static readonly FracFocusDataAdapter instance = new FracFocusDataAdapter();

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private static FracFocusDataAdapter GetInstance()
        {
            return instance;
        }

        internal static FracFocusDataAdapter Instance
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get { return GetInstance(); }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        static FracFocusDataAdapter()
        {
            AppDomain.CurrentDomain.ProcessExit += RrcTexasDataAdapter_Dtor;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private static void RrcTexasDataAdapter_Dtor(object    sender,
                                                     EventArgs e)
        {
            Instance.Dispose();
        }

        internal FracFocusContext DbContext
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            private set;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private FracFocusDataAdapter()
        {
            DbContext = new FracFocusContext();
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public void Dispose()
        {
            DbContext.Dispose();
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static void Initialize(DataStorage dbPath = null)
        {
            Instance.DbContext.Database.EnsureCreated();

            if(dbPath == null)
            {
                return;
            }

            if(Instance.DbContext == null)
            {
                Instance.DbContext = new FracFocusContext(dbPath);
            }
            else if(Instance.DbContext.Database.GetDbConnection().Database != dbPath.FullPath)
            {
                Instance.DbContext.Dispose();

                Instance.DbContext = new FracFocusContext(dbPath);
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static async Task<Registry> GetWellByApi(ApiNumber api)
        {
            try
            {
                Registry record = await Instance.DbContext.Registries.FirstOrDefaultAsync(x => x.ApiNumber == api);

                if(record != null)
                {
                    return record;
                }
            }
            catch(Exception ex)
            {
                Debug.WriteLine(ex.Message);
            }

            return null;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static async Task<bool> Add(Registry registry)
        {
            try
            {
                Registry record = await Instance.DbContext.Registries.FirstOrDefaultAsync(x => x.Id == registry.Id);

                EntityEntry<Registry> entry;

                if(record == null)
                {
                    entry = await Instance.DbContext.Registries.AddAsync(registry);
                }
                else
                {
                    entry = Instance.DbContext.Registries.Update(registry);
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

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static async Task<int> Commit()
        {
            return await Instance.DbContext.SaveChangesAsync();
        }
    }
}