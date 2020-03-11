using System;
using System.Collections;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Data;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;

using Microsoft.Data.Analysis;

using OilGas.Data;
using OilGas.Data.FracFocus;
//using OilGas.Data.RRC.Texas;
using VegaLite;

using System.Data.SqlClient;
using System.Numerics;
using System.Runtime.Serialization.Formatters.Binary;
using System.Threading.Tasks;

using AngleSharp.Html.Dom;

using Microsoft.DotNet.Interactive.Formatting;

using OilGas.Data.RRC.Texas;

namespace RRC.Texas.Driver
{
    internal class Program
    {
        internal static readonly string BoolName = typeof(bool).Name;

        private static void Main(string[] args)
        {
            //IEnumerable<string> organizationNames = QueryBuilder.OrganizationNameQueryByNumber("947128");

            //foreach (string organizationName in organizationNames)
            //{
            //    Console.WriteLine(organizationName);
            //}

            Test5();


            //TestDb0();

            //Test5();

            //TestDb1();
            //TestDb2();

#if DEBUG
            Console.WriteLine("press any key to exit.");
            Console.ReadKey();
#endif
        }

        //private static void TestDb1()
        //{
        //    FracFocusDataAdapter.Initialize(new DataStorage("Rrc.Texas.db"));
        //
        //    string queryString = "SELECT [pKey],"                                                                                             +
        //                         "[JobStartDate],[JobEndDate],[APINumber],[StateNumber],[CountyNumber],[OperatorName],[WellName],[Latitude]," +
        //                         "[Longitude],[Projection],[TVD],[TotalBaseWaterVolume],[TotalBaseNonWaterVolume],[StateName],[CountyName],"  +
        //                         "[FFVersion],[FederalWell],[IndianWell],[Source],[DTMOD]\n"                                                  +
        //                         "FROM [FracFocusRegistry].[dbo].[RegistryUpload]\n"                                                          +
        //                         "WHERE [StateName] = 'Texas'";

        //    //string connectionString = "Server=HELLSCOMPUTER;Database=FracFocusRegistry;Integrated Security=True;";
        //    SqlConnectionStringBuilder connectionBuilder = new SqlConnectionStringBuilder
        //    {
        //        ["Server"]              = "HELLSCOMPUTER",
        //        ["Database"]            = "FracFocusRegistry",
        //        ["Integrated Security"] = "True"
        //    };

        //    using(SqlConnection connection = new SqlConnection(connectionBuilder.ToString()))
        //    {
        //        SqlCommand command = new SqlCommand(queryString,
        //                                            connection);

        //        connection.Open();

        //        using(SqlDataReader reader = command.ExecuteReader())
        //        {
        //            if(reader.HasRows)
        //            {
        //                while(reader.Read())
        //                {
        //                    FracFocusDataAdapter.Add(new Registry(reader));
        //                }
        //            }
        //            else
        //            {
        //                Console.WriteLine("No rows found.");
        //            }
        //        }
        //    }

        //    FracFocusDataAdapter.Commit();
        //}


        private static void TestDb0()
        {
            DataStorage ds = new DataStorage("ConcurrentDictionary.Test");

            Database<int, double[]> db = new Database<int, double[]>(ds);

            db.KeyValues.TryAdd(0,
                                new double[] { 0.2003 });
            db.KeyValues.TryAdd(1,
                                new double[] { 1.2003 });
            db.KeyValues.TryAdd(2,
                                new double[] { 2.2003 });

            db.Save();

            Database<int, double[]> loaded_db = Database<int, double[]>.Load(ds);

            Console.WriteLine(loaded_db.KeyValues[0][0]);
        }

        private static void TestDb2()
        {
            FracFocusDataAdapter.Initialize(new DataStorage("Rrc.Texas.db"));

            Registry entry = FracFocusDataAdapter.GetWellByApi("42-317-39174");

            //foreach(KeyValuePair<Guid, Registry> entry in entries)
            //{
            Console.WriteLine(entry);
            //}
        }

        //private static void TestDb3()
        //{
        //    FracFocusDataAdapter.Initialize();
        //
        //    string queryString = "SELECT [pKey],"                                                                                                      +
        //                         "[pKeyRegistryUpload], [TradeName],[Supplier],[Purpose],[SystemApproach],[IsWater],[PercentHFJob],[IngredientMSDS]\n" +
        //                         "FROM [FracFocusRegistry].[dbo].[RegistryUploadPurpose] AS RegistryPurpose\n"                                         +
        //                         "WHERE EXISTS\n(\n"                                                                                                   +
        //                         "SELECT * FROM [FracFocusRegistry].[dbo].[RegistryUpload] AS Registry\n"                                              +
        //                         "WHERE RegistryPurpose.pKeyRegistryUpload=Registry.pKey AND Registry.StateName='Texas'\n)";
        //
        //    //string connectionString = "Server=HELLSCOMPUTER;Database=FracFocusRegistry;Integrated Security=True;";
        //    SqlConnectionStringBuilder connectionBuilder = new SqlConnectionStringBuilder
        //    {
        //        ["Server"]              = "HELLSCOMPUTER",
        //        ["Database"]            = "FracFocusRegistry",
        //        ["Integrated Security"] = "True"
        //    };
        //
        //    using(SqlConnection connection = new SqlConnection(connectionBuilder.ToString()))
        //    {
        //        SqlCommand command = new SqlCommand(queryString,
        //                                            connection);
        //
        //        connection.Open();
        //
        //        using(SqlDataReader reader = command.ExecuteReader())
        //        {
        //            if(reader.HasRows)
        //            {
        //                while(reader.Read())
        //                {
        //                    FracFocusDataAdapter.Add(new RegistryPurpose(reader));
        //                }
        //            }
        //            else
        //            {
        //                Console.WriteLine("No rows found.");
        //            }
        //        }
        //    }
        //
        //    FracFocusDataAdapter.Commit();
        //}

        //private static void TestDb4()
        //{
        //    FracFocusDataAdapter.Initialize();
        //
        //    string queryString = "SELECT [pKey],"                                                                                        +
        //                         "[pKeyPurpose], [IngredientName],[CASNumber],[PercentHighAdditive],[PercentHFJob],[IngredientComment]," +
        //                         "[IngredientMSDS],[MassIngredient],[ClaimantCompany],[pKeyDisclosure] "                                 +
        //                         "FROM [FracFocusRegistry].[dbo].[RegistryUploadIngredients] "                                           +
        //                         "WHERE [StateName] = 'Texas'";
        //
        //    //string connectionString = "Server=HELLSCOMPUTER;Database=FracFocusRegistry;Integrated Security=True;";
        //    SqlConnectionStringBuilder connectionBuilder = new SqlConnectionStringBuilder
        //    {
        //        ["Server"]              = "HELLSCOMPUTER",
        //        ["Database"]            = "FracFocusRegistry",
        //        ["Integrated Security"] = "True"
        //    };
        //
        //    using(SqlConnection connection = new SqlConnection(connectionBuilder.ToString()))
        //    {
        //        SqlCommand command = new SqlCommand(queryString,
        //                                            connection);
        //
        //        connection.Open();
        //
        //        using(SqlDataReader reader = command.ExecuteReader())
        //        {
        //            if(reader.HasRows)
        //            {
        //                while(reader.Read())
        //                {
        //                    FracFocusDataAdapter.Add(new RegistryIngredients(reader));
        //                }
        //            }
        //            else
        //            {
        //                Console.WriteLine("No rows found.");
        //            }
        //        }
        //    }
        //
        //    FracFocusDataAdapter.Commit();
        //}

        //private static void FillFracFocusDb()
        //{
        //    //FracFocusDataAdapter.RegistryUploadCsvToDb(@"T:\registryupload_1.csv", @"C:\Users\tehgo\FracFocus.db");
        //    //FracFocusDataAdapter.RegistryUploadCsvToDb(@"T:\registryupload_2.csv", @"C:\Users\tehgo\FracFocus.db");
        //    ////FracFocusDataAdapter.RegistryUploadCsvToDb(@"T:\FracFocusRegistry_1.csv");
        //}

        //private static void TestFracFocus()
        //{
        // FracFocusDataAdapter.Initialize(new DataStorage("FracFocus.db"));
        //
        // //448C1DAB-C7FD-4E07-9D6F-E3B1CF64B708
        //
        // Registry registry = FracFocusDataAdapter.GetWellByApi("42317372620000").Result;
        //}

        //private static void Test1()
        //{
        //    RrcTexasDataAdapter.Initialize();

        //    WellProduction wellProduction = RrcTexasDataAdapter.GetProductionByApi("42-317-39174").Result;

        //    RrcTexasDataAdapter.Commit();
        //}

        //private static void Test2()
        //{
        //    RrcTexasDataAdapter.Initialize();

        //    WellProduction wellProduction = RrcTexasDataAdapter.GetProductionByApi("42-317-39174").Result;

        //    RrcTexasDataAdapter.Commit();

        //    DataFrame dataFrame = wellProduction.ToDataFrame();

        //    foreach(DataFrameRow entry in dataFrame.Rows)
        //    {
        //        Console.WriteLine(entry);
        //    }
        //}

        ////private static void Test2()
        ////{
        ////    RrcTexasDataAdapter.Initialize();
        ////    string api = "42-12332309";

        ////    IEnumerable<Lease> leases = RrcTexasDataAdapter.GetLeaseByApi(api).Result;

        ////    foreach(Lease lease in leases)
        ////    {
        ////        Console.WriteLine(lease);
        ////    }
        ////}

        //private static void Test3()
        //{
        //    string api = "42-285-33615";

        //    RrcTexasDataAdapter.Initialize();

        //    WellProduction wellProduction = RrcTexasDataAdapter.GetProductionByApi(api).Result;

        //    DataFrame dataFrame = wellProduction.ToDataFrame();

        //    PrimitiveDataFrameColumn<int>   month      = (PrimitiveDataFrameColumn<int>)dataFrame["Month"];
        //    PrimitiveDataFrameColumn<float> monthlyOil = (PrimitiveDataFrameColumn<float>)dataFrame["MonthlyOil"];
        //}

        ////private static void Test4()
        ////{
        ////    string api = "42-285-33615";
        ////
        ////    WellProduction wellProduction = RrcTexasDataAdapter.GetProductionByApi(api).Result;
        ////
        ////    DataFrame dataFrame = wellProduction.ToDataFrame();
        ////
        ////    PrimitiveDataFrameColumn<int>   month      = (PrimitiveDataFrameColumn<int>)dataFrame["Month"];
        ////    PrimitiveDataFrameColumn<float> monthlyOil = (PrimitiveDataFrameColumn<float>)dataFrame["MonthlyOil"];
        ////
        ////    string spec_json = "{\n"                                                                     +
        ////                       "    \"$schema\": \"https://vega.github.io/schema/vega-lite/v4.json\",\n" +
        ////                       "    \"description\": \"Stock prices of 5 Tech Companies over Time.\",\n" +
        ////                       "    \"data\": {\n"                                                       +
        ////                       "        \"values\": []\n"                                                +
        ////                       "    },\n"                                                                +
        ////                       "    \"mark\": {\n"                                                       +
        ////                       "        \"type\": \"line\",\n"                                           +
        ////                       "        \"point\": {\n"                                                  +
        ////                       "            \"filled\": false,\n"                                        +
        ////                       "            \"fill\": \"white\"\n"                                       +
        ////                       "        }\n"                                                             +
        ////                       "    },\n"                                                                +
        ////                       "    \"encoding\": {\n"                                                   +
        ////                       "        \"x\": {\n"                                                      +
        ////                       "            \"timeUnit\": \"year\",\n"                                   +
        ////                       "            \"field\": \"date\",\n"                                      +
        ////                       "            \"type\": \"temporal\"\n"                                    +
        ////                       "        },\n"                                                            +
        ////                       "        \"y\": {\n"                                                      +
        ////                       "            \"aggregate\": \"mean\",\n"                                  +
        ////                       "            \"field\": \"price\",\n"                                     +
        ////                       "            \"type\": \"quantitative\"\n"                                +
        ////                       "        },\n"                                                            +
        ////                       "        \"color\": {\n"                                                  +
        ////                       "            \"field\": \"symbol\",\n"                                    +
        ////                       "            \"type\": \"nominal\"\n"                                     +
        ////                       "        }\n"                                                             +
        ////                       "    }\n"                                                                 +
        ////                       "}";
        ////
        ////    //var vegaLiteSpecification = VegaLiteSpecification.FromJson(spec_json);
        ////
        ////    //vegaLiteSpecification.Data.Values = rows;
        ////
        ////    VegaLiteSpecification vegaLiteSpecification = new VegaLiteSpecification
        ////    {
        ////        Description = "Stock prices of 5 Tech Companies over Time.",
        ////        Data = new UrlData()
        ////        {
        ////            Values = TestData.VegaDataset
        ////        },
        ////        Mark = new BoxPlotDefClass()
        ////        {
        ////            Type = BoxPlot.Line,
        ////            Point = new OverlayMarkDef()
        ////            {
        ////                Filled = false, Fill = "white"
        ////            }
        ////        },
        ////        Encoding = new Encoding()
        ////        {
        ////            Color = new DefWithConditionMarkPropFieldDefGradientStringNull()
        ////            {
        ////                Type = StandardType.Nominal, Field = "symbol"
        ////            },
        ////            X = new XClass()
        ////            {
        ////                Type     = StandardType.Temporal,
        ////                TimeUnit = TimeUnit.Year,
        ////                Field    = "date"
        ////            },
        ////            Y = new YClass()
        ////            {
        ////                Type      = StandardType.Quantitative,
        ////                Field     = "price",
        ////                Aggregate = NonArgAggregateOp.Mean
        ////            }
        ////        }
        ////    };
        ////
        ////    Chart chart = new Chart("Stock prices of 5 Tech Companies over Time.",
        ////                            vegaLiteSpecification,
        ////                            500,
        ////                            500);
        ////
        ////    chart.ShowInBrowser();
        ////}

        private static void Test5()
        {
            ApiNumber api = "42-285-33615";

            RrcTexasDataAdapter.Initialize(new DataStorage("Rrc.Texas.db"));

            WellProduction wellProduction = RrcTexasDataAdapter.GetProductionByApi(api,
                                                                                   true).Result;

            RrcTexasDataAdapter.Commit();

            Chart chart = wellProduction.BuildChart();

            chart.ShowInBrowser();

            //Console.WriteLine(chart.ToString());
            //Console.ReadKey();
        }
    }
}

//string api = "42-285-33615".Replace("-", "");

// using LiteDatabase database = new LiteDatabase("C:/Users/tehgo/RRC.Texas.db");
// LiteCollection<SpecificLeaseProductionQueryData> collection = database.GetCollection<SpecificLeaseProductionQueryData>(nameof(SpecificLeaseProductionQueryData));
//
// SpecificLeaseProductionQueryData[] records = collection.Query().Where(Query.EQ("API",
//                                                                                api)).ToArray();