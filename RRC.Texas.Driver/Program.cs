using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;

using Microsoft.Data.Analysis;

using OilGas.Data;
using OilGas.Data.FracFocus;
using OilGas.Data.RRC.Texas;

using VegaLite;

namespace RRC.Texas.Driver
{
    internal class Program
    {
        internal static readonly string BoolName = typeof(bool).Name;

        private static void Main(string[] args)
        {
            Test5();
#if DEBUG
            Console.WriteLine("press any key to exit.");
            Console.ReadKey();
#endif
        }

        private static void FillFracFocusDb()
        {
            //FracFocusDataAdapter.RegistryUploadCsvToDb(@"T:\registryupload_1.csv", @"C:\Users\tehgo\FracFocus.db");
            //FracFocusDataAdapter.RegistryUploadCsvToDb(@"T:\registryupload_2.csv", @"C:\Users\tehgo\FracFocus.db");
            ////FracFocusDataAdapter.RegistryUploadCsvToDb(@"T:\FracFocusRegistry_1.csv");
        }

        private static void Test1()
        {
            foreach(string api in TestData.apis)
            {
                WellProduction wellProduction = RrcTexasDataAdapter.GetProductionByApi(api).Result;

                DataFrame dataFrame = wellProduction.ToDataFrame();

                foreach(DataFrameRow entry in dataFrame.Rows)
                {
                    Console.WriteLine(entry);
                }
            }
        }

        private static void Test2()
        {
            string api = "42-12332309";

            IEnumerable<Lease> leases = RrcTexasDataAdapter.GetLeaseByApi(api).Result;

            foreach(Lease lease in leases)
            {
                Console.WriteLine(lease);
            }
        }

        private static void Test3()
        {
            string api = "42-285-33615";

            WellProduction wellProduction = RrcTexasDataAdapter.GetProductionByApi(api).Result;

            DataFrame dataFrame = wellProduction.ToDataFrame();

            PrimitiveDataFrameColumn<int>   month      = (PrimitiveDataFrameColumn<int>)dataFrame["Month"];
            PrimitiveDataFrameColumn<float> monthlyOil = (PrimitiveDataFrameColumn<float>)dataFrame["MonthlyOil"];
        }

        private static void Test4()
        {
            string api = "42-285-33615";

            WellProduction wellProduction = RrcTexasDataAdapter.GetProductionByApi(api).Result;

            DataFrame dataFrame = wellProduction.ToDataFrame();

            PrimitiveDataFrameColumn<int>   month      = (PrimitiveDataFrameColumn<int>)dataFrame["Month"];
            PrimitiveDataFrameColumn<float> monthlyOil = (PrimitiveDataFrameColumn<float>)dataFrame["MonthlyOil"];

            string spec_json = "{\n"                                                                     +
                               "    \"$schema\": \"https://vega.github.io/schema/vega-lite/v4.json\",\n" +
                               "    \"description\": \"Stock prices of 5 Tech Companies over Time.\",\n" +
                               "    \"data\": {\n"                                                       +
                               "        \"values\": []\n"                                                +
                               "    },\n"                                                                +
                               "    \"mark\": {\n"                                                       +
                               "        \"type\": \"line\",\n"                                           +
                               "        \"point\": {\n"                                                  +
                               "            \"filled\": false,\n"                                        +
                               "            \"fill\": \"white\"\n"                                       +
                               "        }\n"                                                             +
                               "    },\n"                                                                +
                               "    \"encoding\": {\n"                                                   +
                               "        \"x\": {\n"                                                      +
                               "            \"timeUnit\": \"year\",\n"                                   +
                               "            \"field\": \"date\",\n"                                      +
                               "            \"type\": \"temporal\"\n"                                    +
                               "        },\n"                                                            +
                               "        \"y\": {\n"                                                      +
                               "            \"aggregate\": \"mean\",\n"                                  +
                               "            \"field\": \"price\",\n"                                     +
                               "            \"type\": \"quantitative\"\n"                                +
                               "        },\n"                                                            +
                               "        \"color\": {\n"                                                  +
                               "            \"field\": \"symbol\",\n"                                    +
                               "            \"type\": \"nominal\"\n"                                     +
                               "        }\n"                                                             +
                               "    }\n"                                                                 +
                               "}";

            //var vegaLiteSpecification = VegaLiteSpecification.FromJson(spec_json);

            //vegaLiteSpecification.Data.Values = rows;

            VegaLiteSpecification vegaLiteSpecification = new VegaLiteSpecification
            {
                Description = "Stock prices of 5 Tech Companies over Time.",
                Data = new UrlData()
                {
                    Values = TestData.VegaDataset
                },
                Mark = new BoxPlotDefClass()
                {
                    Type = BoxPlot.Line,
                    Point = new OverlayMarkDef()
                    {
                        Filled = false, Fill = "white"
                    }
                },
                Encoding = new Encoding()
                {
                    Color = new DefWithConditionMarkPropFieldDefGradientStringNull()
                    {
                        Type = StandardType.Nominal, Field = "symbol"
                    },
                    X = new XClass()
                    {
                        Type     = StandardType.Temporal,
                        TimeUnit = TimeUnit.Year,
                        Field    = "date"
                    },
                    Y = new YClass()
                    {
                        Type      = StandardType.Quantitative,
                        Field     = "price",
                        Aggregate = NonArgAggregateOp.Mean
                    }
                }
            };

            Chart chart = new Chart("Stock prices of 5 Tech Companies over Time.",
                                    vegaLiteSpecification,
                                    500,
                                    500);

            chart.ShowInBrowser();
        }

        private static void Test5()
        {
            ApiNumber api = "42-285-33615";

            WellProduction wellProduction = RrcTexasDataAdapter.GetProductionByApi(api,
                                                                                   true).Result;

            Chart chart = wellProduction.BuildChart();

            chart.ShowInBrowser();

            Console.WriteLine(chart.ToString());
            Console.ReadKey();
        }
    }
}

//string api = "42-285-33615".Replace("-", "");

// using LiteDatabase database = new LiteDatabase("C:/Users/tehgo/RRC.Texas.db");
// LiteCollection<SpecificLeaseProductionQueryData> collection = database.GetCollection<SpecificLeaseProductionQueryData>(nameof(SpecificLeaseProductionQueryData));
//
// SpecificLeaseProductionQueryData[] records = collection.Query().Where(Query.EQ("API",
//                                                                                api)).ToArray();