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

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        private static FracFocusDataAdapter GetInstance()
        {
            return instance;
        }

        internal static FracFocusDataAdapter Instance
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
        static FracFocusDataAdapter()
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

        internal FracFocusContext DbContext
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
        private FracFocusDataAdapter()
        {
            DbContext = new FracFocusContext();
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void Dispose()
        {
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
                Instance.DbContext = new FracFocusContext(dbPath);
            }
            else if(Instance.DbContext.Database.GetDbConnection().Database != dbPath.FullPath)
            {
                Instance.DbContext.Dispose();

                Instance.DbContext = new FracFocusContext(dbPath);
            }
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
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

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
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

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public static async Task<int> Commit()
        {
            return await Instance.DbContext.SaveChangesAsync();
        }
    }
}