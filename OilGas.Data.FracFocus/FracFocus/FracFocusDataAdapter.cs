using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading.Tasks;

namespace OilGas.Data.FracFocus
{
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
            throw new Exception("OilGas.Data.FracFocus.FracFocusDataAdapter is not working until the FracFocus.db is rebuilt.");

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
        public static void Initialize(DataStorage dbPath)
        {
            //Instance.DbContext.Database.EnsureCreated();

            if(dbPath == null)
            {
                return;
            }

            if(Instance.DbContext == null)
            {
                Instance.DbContext = new FracFocusContext(dbPath);
            }
            else if(Instance.DbContext.DataStorage.FullPath != dbPath.FullPath)
            {
                Instance.DbContext.Dispose();

                Instance.DbContext = new FracFocusContext(dbPath);
            }

            //if(Instance.DbContext.Registries == null)
            //{
            //    Instance.DbContext.CreateTable<Registry>();
            //}
        }

        public static List<Registry> All()
        {
            return Instance.DbContext.Registries.Values.ToList();
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public static Registry GetWellByApi(ApiNumber api)
        {
            try
            {
                if(Instance.DbContext.Registries.TryGetValue(api.ToString(),
                                                             out Registry record))
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
        public static async Task<IEnumerable<Registry>> GetWellsByOperatorName(string operatorName)
        {
            try
            {
                IEnumerable<Registry> record = await Task.Run(() => Instance.DbContext.Registries.Values.AsParallel().Where(x => x.OperatorName == operatorName));

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
                if(Instance.DbContext.Registries.ContainsKey(registry.ApiNumber))
                {
                    return await Instance.DbContext.UpdateAsync(registry);
                }

                return await Instance.DbContext.InsertAsync(registry);
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
        public static async Task Commit()
        {
            await Task.Run(() => Instance.DbContext.Save());
        }
    }
}