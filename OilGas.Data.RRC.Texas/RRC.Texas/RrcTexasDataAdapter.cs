using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Threading.Tasks;

using AngleSharp.Html.Dom;

using LiteDB;

namespace OilGas.Data.RRC.Texas
{
    public abstract class RrcTexasDataAdapter
    {
        private RrcTexasDataAdapter()
        {
        }

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

        public static async Task<WellProduction> GetProductionByApi(ApiNumber   api,
                                                                    bool        persistentData = false,
                                                                    DataStorage dbPath         = default)
        {
            if(persistentData)
            {
                PersistentData.Initialize(dbPath);

                try
                {
                    PersistentData.InitializeCollection<WellProductionRecord>();
                    PersistentData.InitializeCollection<WellProduction, List<WellProductionRecord>>(x => x.Records);

                    BsonExpression queryApi = Query.EQ("Api",
                                                       api.ToString());

                    if(await PersistentData.Contains<WellProduction, List<WellProductionRecord>>(queryApi))
                    {
                        WellProduction records = PersistentData.FindBy<WellProduction, List<WellProductionRecord>>(queryApi).FirstOrDefault();

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
                await PersistentData.AddRange(wellProduction.Records);

                await PersistentData.Add<WellProduction, List<WellProductionRecord>>(wellProduction);
            }

            return wellProduction;
        }
    }
}