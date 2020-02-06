using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading.Tasks;

using AngleSharp.Html.Dom;

namespace OilGas.Data.RRC.Texas
{
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
            //DbContext.SaveChanges();
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

            // IDbCommand dbExist = Instance.DbContext.CreateCommand();
            //
            // dbExist.CommandText=$"IF OBJECT_ID(\"{nameof(WellProduction)}\") IS NOT NULL";
            //
            // var result = dbExist.ExecuteScalar();

            if(Instance.DbContext == null)
            {
                Instance.DbContext = new RrcTexasContext(dbPath);
            }
            else if(Instance.DbContext.DataStorage.FullPath != dbPath.FullPath)
            {
                Instance.DbContext.Dispose();

                Instance.DbContext = new RrcTexasContext(dbPath);
            }

            //if(Instance.DbContext.WellProductionRecords == null)
            //{
            //    Instance.DbContext.CreateTable<WellProductionRecord>();
            //}

            //if(Instance.DbContext.WellProductions == null)
            //{
            //    Instance.DbContext.CreateTable<WellProduction>();
            //}
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
                if(Instance.DbContext.WellProductions.TryGetValue(wellProduction, out WellProduction record))
                {
                    return await Instance.DbContext.UpdateAsync(record, wellProduction);
                }
                return await Instance.DbContext.InsertAsync(wellProduction);
            }
            catch(Exception ex)
            {
                Debug.WriteLine(ex.Message);
            }

            return false;
        }

//#if NETCOREAPP
//        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
//#else
//        [MethodImpl(MethodImplOptions.AggressiveInlining)]
//#endif
//        public static async Task<bool> Add(WellProductionRecord wellProductionRecord)
//        {
//            try
//            {
//                WellProductionRecord record =
//                    await Task.Run(() => Instance.DbContext.WellProductions.FirstOrDefault(x => x.WellProduction == wellProductionRecord.WellProduction &&
//                                                                                                      x.Month          == wellProductionRecord.Month));

//                int entry;

//                if(record == null)
//                {
//                    entry = await Instance.DbContext.InsertAsync(wellProductionRecord);
//                }
//                else
//                {
//                    entry = await Instance.DbContext.UpdateAsync(wellProductionRecord);
//                }

//                if(entry == 1)
//                {
//                    return true;
//                }
//            }
//            catch(Exception ex)
//            {
//                Debug.WriteLine(ex.Message);
//            }

//            return false;
//        }

//#if NETCOREAPP
//        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
//#else
//        [MethodImpl(MethodImplOptions.AggressiveInlining)]
//#endif
//        public static async Task<bool> AddRange(IEnumerable<WellProductionRecord> wellProductionRecords)
//        {
//            bool result = true;

//            foreach(WellProductionRecord record in wellProductionRecords)
//            {
//                result &= await Add(record);
//            }

//            return result;
//        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public static async Task Commit()
        {
            await Task.Run(() => Instance.DbContext.Save());
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
                    WellProduction records = await Task.Run(() => Instance.DbContext.WellProductions.AsParallel().FirstOrDefault(x => x.Api == api));

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

                //await AddRange(wellProduction.Records);
            }

            return wellProduction;
        }
    }
}