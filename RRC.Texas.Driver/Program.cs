using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Data;
using System.IO;
using System.Linq;
using System.Net;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Security.Cryptography.X509Certificates;
using System.Threading.Tasks;

using Windows.Data.Pdf;
using Windows.Globalization;
using Windows.Graphics.Imaging;
using Windows.Media.Ocr;
using Windows.Storage;
using Windows.Storage.Streams;

using Engineering.DataSource;
using Engineering.DataSource.CoordinateSystems;
using Engineering.DataSource.OilGas;
using Engineering.DataSource.Tools;

using GeoSpatial;

using Kokkos;

using OilGas.Data;
using OilGas.Data.RRC.Texas;

namespace RRC.Texas.Driver
{
    internal class Program
    {
        private const string OG_LEASE_CYCLE_DATA_HEADER = @"""OIL_GAS_CODE"", ""DISTRICT_NO"", ""LEASE_NO"", "                          +
                                                          @"""CYCLE_YEAR"", ""CYCLE_MONTH"", ""CYCLE_YEAR_MONTH"", "                    +
                                                          @"""LEASE_NO_DISTRICT_NO"", ""OPERATOR_NO"", ""FIELD_NO"", "                  +
                                                          @"""FIELD_TYPE"", ""GAS_WELL_NO"", ""PROD_REPORT_FILED_FLAG"", "              +
                                                          @"""LEASE_OIL_PROD_VOL"", ""LEASE_OIL_ALLOW"", ""LEASE_OIL_ENDING_BAL"", "    +
                                                          @"""LEASE_GAS_PROD_VOL"", ""LEASE_GAS_ALLOW"", ""LEASE_GAS_LIFT_INJ_VOL"", "  +
                                                          @"""LEASE_COND_PROD_VOL"", ""LEASE_COND_LIMIT"", ""LEASE_COND_ENDING_BAL"", " +
                                                          @"""LEASE_CSGD_PROD_VOL"", ""LEASE_CSGD_LIMIT"", ""LEASE_CSGD_GAS_LIFT"", "   +
                                                          @"""LEASE_OIL_TOT_DISP"", ""LEASE_GAS_TOT_DISP"", ""LEASE_COND_TOT_DISP"", "  +
                                                          @"""LEASE_CSGD_TOT_DISP"", ""DISTRICT_NAME"", ""LEASE_NAME"", "               +
                                                          @"""OPERATOR_NAME"", ""FIELD_NAME""";

        internal static readonly string BoolName = typeof(bool).Name;

        private static void QueryProductionByApi()
        {
            {
                //RrcTexasDataAdapter adapter = new RrcTexasDataAdapter();

                //adapter.LoadDrillingPermitsCsv(@"C:\Users\tehgo\Desktop\DrillingPermits_STX_2000_2020.csv");

                //adapter.Backup();
            }

            {
                //RrcTexasDataAdapter adapter = new RrcTexasDataAdapter();

                //adapter.LoadDb(@"D:\TFS_Sources\Github\Compilation\trmcnealy\OilGas.Data\RRC.Texas.Driver\bin\Debug\netcoreapp3.1\OilGas.db");

                //bool FilterWells(DrillingPermit dp)
                //{
                //    if(dp.County != "KARNES")
                //    {
                //        return false;
                //    }

                //    if(!(dp.WellboreProfile.Contains("Horizontal"))) //|| dp.WellboreProfile.Contains("Directional")
                //    {
                //        return false;
                //    }

                //    return true;
                //}

                //List<DrillingPermit> drillingPermits = adapter.GetDrillingPermits().Where(FilterWells).ToList();

                ////float[] totalDepths = drillingPermits.Select(dp => float.Parse(dp.TotalDepth)).ToArray();

                //Well well;

                //int counter = 0;

                //foreach(DrillingPermit drillingPermit in drillingPermits)
                //{
                //    well = null;

                //    try
                //    {
                //        well = adapter.GetMonthlyProductionByApi(drillingPermit.Api).Result;
                //    }
                //    catch(Exception)
                //    {
                //        //
                //    }

                //    try
                //    {
                //        if(well != null)
                //        {
                //            well = adapter.GetG1Report(well).Result;
                //        }
                //    }
                //    catch(Exception)
                //    {
                //        //
                //    }

                //    try
                //    {
                //        if(well != null)
                //        {
                //            well = adapter.GetDirectionalSurvey(well).Result;
                //        }
                //    }
                //    catch(Exception)
                //    {
                //        //
                //    }

                //    //if(well != null)
                //    //{
                //    //    well.DrillingPermit = drillingPermit;

                //    //    adapter.Update(well);

                //    //    Console.WriteLine(well.Api);
                //    //    ++counter;
                //    //}

                //    if(counter % 10 == 0)
                //    {
                //        adapter.Backup();
                //    }
                //}

                //adapter.Backup();
            }
        }

        private static void QueryReportsByApi()
        {
            // RrcTexasDataAdapter adapter = new RrcTexasDataAdapter();
            //
            // adapter.LoadDb(@"R:\OilGasData\OilGas.db");
            //
            // foreach(Well well in adapter.GetAllWells().Where(w => w.MonthlyProduction.Records.Count > 3))
            // {
            //     try
            //     {
            //         if(well != null)
            //         {
            //             Well result = adapter.GetReports(well).Result;
            //         }
            //     }
            //     catch(Exception)
            //     {
            //         //
            //     }
            // }
        }

        private static void DbFixes()
        {
            //RrcTexasDataAdapter adapter = new RrcTexasDataAdapter();

            //adapter.LoadDb(@"D:\TFS_Sources\Github\Compilation\trmcnealy\OilGas.Data\RRC.Texas.Driver\bin\Debug\netcoreapp3.1\OilGas.db");

            //adapter.DbFixes();

            //adapter.Commit();

            //adapter.Backup();
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private static IReadOnlyList<OcrLine> ReadPdf(string pdfFile)
        {
            StorageFile file = StorageFile.GetFileFromPathAsync(pdfFile).GetAwaiter().GetResult();

            PdfDocument pdfDocument = PdfDocument.LoadFromFileAsync(file).GetAwaiter().GetResult();

            if(pdfDocument == null)
            {
                throw new NullReferenceException(nameof(pdfDocument));
            }

            uint pageCount = pdfDocument.PageCount;

            if(pageCount >= 1)
            {
                using(PdfPage page = pdfDocument.GetPage(0))
                {
                    Language  ocrLanguage = new Language("en-US");
                    OcrEngine ocrEngine   = OcrEngine.TryCreateFromUserProfileLanguages() ?? OcrEngine.TryCreateFromLanguage(ocrLanguage);

                    InMemoryRandomAccessStream stream = new InMemoryRandomAccessStream();

                    page.RenderToStreamAsync(stream).GetAwaiter().GetResult();

                    BitmapDecoder decoder = BitmapDecoder.CreateAsync(stream).GetAwaiter().GetResult();

                    SoftwareBitmap bitmap = decoder.GetSoftwareBitmapAsync().GetAwaiter().GetResult(); //.GetSoftwareBitmapAsync(BitmapPixelFormat.Rgba16, BitmapAlphaMode.Straight);

                    Console.WriteLine($"{pdfFile}:{bitmap.PixelWidth}x{bitmap.PixelHeight}");

                    OcrResult ocrResult = ocrEngine.RecognizeAsync(bitmap).GetAwaiter().GetResult();

                    IReadOnlyList<OcrLine> lines = ocrResult.Lines;

                    return lines;
                }
            }

            return null;
        }

        private static void LoadCsvData()
        {
            //     using(RrcTexasDataAdapter adapter = new RrcTexasDataAdapter())
            //     {
            //         adapter.LoadDrillingPermitsCsv(@"R:\DrillingPermits_STX_2000_2020.csv");
            //
            //         adapter.LoadFracFocusRegistryCsv(@"R:\registryupload_1.csv");
            //
            //         adapter.LoadFracFocusRegistryCsv(@"R:\registryupload_2.csv");
            //
            //         adapter.Optimize();
            //
            //         adapter.Compact();
            //
            //         adapter.Backup();
            //     }
        }

        //public static Location GetLocationByApi(List<Registry> fracFocusRegistry,
        //                                        ApiNumber      api)
        //{
        //    try
        //    {
        //        Registry record = fracFocusRegistry.First(w => w.ApiNumber == api);

        //        if(record != null)
        //        {
        //            return new Location(record.Latitude, record.Longitude, record.Projection);
        //        }
        //    }
        //    catch(Exception ex)
        //    {
        //        Debug.WriteLine(ex.Message);
        //    }

        //    return null;
        //}

        public static void GetWellLocations()
        {
            // List<DrillingPermit> drillingPermits;
            // List<Registry>       fracFocusRegistry;
            //
            // using(RrcTexasDataAdapter adapter = new RrcTexasDataAdapter())
            // {
            //     adapter.LoadDb(@"D:\TFS_Sources\Github\Compilation\trmcnealy\OilGas.Data\RRC.Texas.Driver\bin\Debug\netcoreapp3.1\Rrc.Texas.db");
            //
            //     bool FilterWells(DrillingPermit dp)
            //     {
            //         if(dp.County != "KARNES")
            //         {
            //             return false;
            //         }
            //
            //         if(!(dp.WellboreProfile.Contains("Horizontal") || dp.WellboreProfile.Contains("Directional")))
            //         {
            //             return false;
            //         }
            //
            //         return true;
            //     }
            //
            //     // drillingPermits = adapter.DrillingPermits.Where(FilterWells).ToList();
            //     //
            //     // fracFocusRegistry = context.FracFocusRegistry.ToList();
            // }
            //
            // //(ApiNumber, WellLocation)[] locations = new (ApiNumber, WellLocation)[drillingPermits.Count];
            //
            // //Parallel.ForEach(Partitioner.Create(0, drillingPermits.Count),
            // //                 (row,
            // //                  loopState) =>
            // //                 {
            // //                     for(int i = row.Item1; i < row.Item2; i++)
            // //                     {
            // //                         locations[i] = (drillingPermits[i].Api, GetLocationByApi(fracFocusRegistry, drillingPermits[i].Api));
            // //                     }
            // //                 });
            //
            // //var not_null_locations = locations.Where(l => l.Item2 != null);
            //
            // // //double maxLatitude = not_null_locations.Max(l => l.Item2.LatitudeLongitude.Latitude);
            // // //Console.WriteLine($"maxLatitude : {maxLatitude}");
            // // //double minLatitude = not_null_locations.Min(l => l.Item2.LatitudeLongitude.Latitude);
            // // //Console.WriteLine($"minLatitude : {minLatitude}");
            // // double avgLatitude = not_null_locations.Average(l => l.Item2.LatitudeLongitude.Latitude);
            // // Console.WriteLine($"avgLatitude : {avgLatitude}");
            //
            // // //double maxLongitude = not_null_locations.Max(l => l.Item2.LatitudeLongitude.Longitude);
            // // //Console.WriteLine($"maxLongitude : {maxLongitude}");
            // // //double minLongitude = not_null_locations.Min(l => l.Item2.LatitudeLongitude.Longitude);
            // // //Console.WriteLine($"minLongitude : {minLongitude}");
            // // double avgLongitude = not_null_locations.Average(l => l.Item2.LatitudeLongitude.Longitude);
            // // Console.WriteLine($"avgLongitude : {avgLongitude}");
            //
            // //LatitudeLongitude midpoint = new LatitudeLongitude(avgLatitude, avgLongitude);
            //
            // //UniversalTransverseMercator midpointUTM = (UniversalTransverseMercator)midpoint;
            //
            // //using(StreamWriter sw = new StreamWriter("LatLongs.csv"))
            // //{
            // //    sw.WriteLine("Api,Latitude,Longitude,Projection,DistanceFromCenter");
            //
            // //    LatitudeLongitude wellPoint;
            // //    UniversalTransverseMercator wellPointUTM;
            // //    double distance;
            //
            // //    foreach((ApiNumber api, WellLocation location) data in not_null_locations)
            // //    {
            // //        wellPoint = data.location.LatitudeLongitude;
            // //        wellPointUTM = (UniversalTransverseMercator)wellPoint;
            // //        distance = (wellPointUTM - midpointUTM) / 5280.0;
            //
            // //        sw.WriteLine($"{data.api},{data.location.LatitudeLongitude.Latitude},{data.location.LatitudeLongitude.Longitude},{data.location.Projection},{distance}");
            // //    }
            // //}
        }

        public static void TestPvt()
        {
            double bottomHoleTemperature = 416;
            double oilApiGravity         = 62.3;
            double GasSpecificGravity    = 0.742;

            double mu_od = Pvt.OilViscosity.Dead.Beal(bottomHoleTemperature, oilApiGravity);

            double Rs = Pvt.GasSolubility.VasquezBeggs(4815.0, 74.0, oilApiGravity, GasSpecificGravity);

            double A = 0.1651 + 0.6165 * Math.Pow(10.0, -6.0866e-4 * Rs);
            double B = 0.5131 + 0.5109 * Math.Pow(10.0, -1.1831e-3 * Rs);

            double mu = A * Math.Pow(mu_od, B);
        }

        public static void ToCSV(DataTable dtDataTable,
                                 string    strFilePath)
        {
            StreamWriter sw = new StreamWriter(strFilePath, false);

            //headers  
            for(int i = 0; i < dtDataTable.Columns.Count; i++)
            {
                sw.Write(dtDataTable.Columns[i]);

                if(i < dtDataTable.Columns.Count - 1)
                {
                    sw.Write(",");
                }
            }

            sw.Write(sw.NewLine);

            foreach(DataRow dr in dtDataTable.Rows)
            {
                for(int i = 0; i < dtDataTable.Columns.Count; i++)
                {
                    if(!Convert.IsDBNull(dr[i]))
                    {
                        string value = dr[i].ToString();

                        if(value.Contains(','))
                        {
                            value = string.Format("\"{0}\"", value);
                            sw.Write(value);
                        }
                        else
                        {
                            sw.Write(dr[i].ToString());
                        }
                    }

                    if(i < dtDataTable.Columns.Count - 1)
                    {
                        sw.Write(",");
                    }
                }

                sw.Write(sw.NewLine);
            }

            sw.Close();
        }

        public static void ProcessDbase()
        {
            // well
            // "R:/*b.dbf" Bottom Well points
            // "R:/*l.dbf" Surface/Bottom lines
            // "R:/*s.dbf" Surface Well points

            // surv
            // "R:/*Abspt.dbf" abstract points
            // "R:/*l.dbf" Survey lines
            // "R:/*Labpt.dbf" label points
            // "R:/*b.dbf" polygons
            // "R:/*p.dbf" polygons

            string[] dbfTypes = {"*b.dbf", "*l.dbf", "*s.dbf"};

            string[] files;

            foreach(string dbfType in dbfTypes)
            {
                files = Directory.GetFiles("R:/dbase", dbfType);

                DataTable dt;
                DataTable dt_out = null;

                foreach(string file in files)
                {
                    dt = Dbase.Load(file);

                    for(int i = dt.Rows.Count - 1; i >= 0; --i)
                    {
                        if(dt.Rows[i]["API"] is string str && str.TrimEnd().Length < 8)
                        {
                            dt.Rows.RemoveAt(i);
                        }
                    }

                    if(dt_out == null)
                    {
                        dt_out = dt;
                    }
                    else
                    {
                        dt_out.Merge(dt);
                    }
                }

                ToCSV(dt_out, Path.Combine("R:/dbase", dbfType.Substring(1) + ".csv"));
            }
        }

        public static async Task LoadBase()
        {
            //RrcTexasDataAdapter adapter = new RrcTexasDataAdapter();

            //int num_threads = 4;
            //int num_numa    = 1;
            //int device_id   = 0;
            //int ndevices    = 1;
            //int skip_device = 9999;

            //InitArguments arguments = new InitArguments(num_threads, num_numa, device_id, ndevices, skip_device, false);

            //List<string[]> rows;

            //using(ScopeGuard.Get(arguments))
            //{
            //    using(MemoryMapped mm = new MemoryMapped("R:/s.dbf.csv"))
            //    {
            //        MappedCsvReader csvReader = new MappedCsvReader(mm);

            //        (_, rows) = csvReader.ReadFile(1);
            //    }
            //}

            //Dictionary<ApiNumber, WellS> wellS = new Dictionary<ApiNumber, WellS>();

            //WellS well;

            //foreach(string[] row in rows)
            //{
            //    if(row[2].Contains(" "))
            //    {
            //        continue;
            //    }

            //    well = new WellS(row);

            //    if(!well.API.IsInCounty(CountyType.Kind.KARNES))
            //    {
            //        continue;
            //    }

            //    if(!wellS.TryAdd(well.API, well))
            //    {
            //        Console.WriteLine($"Updating {well.API}");
            //        wellS[well.API] = well;
            //    }
            //}

            //await adapter.UpdateLocationAsync(wellS);

            await Task.FromResult(Task.CompletedTask);
        }

        public static void LoadWellDbf()
        {
            //int num_threads = 4;
            //int num_numa = 1;
            //int device_id = 0;
            //int ndevices = 1;
            //int skip_device = 9999;

            //InitArguments arguments = new InitArguments(num_threads, num_numa, device_id, ndevices, skip_device, false);

            //List<string[]> l_rows, s_rows, b_rows;

            //using (ScopeGuard.Get(arguments))
            //{
            //    //using (MemoryMapped l_mm = new MemoryMapped("R:/dbase/l.dbf.csv"))
            //    using (MemoryMapped s_mm = new MemoryMapped("R:/dbase/s.dbf.csv"))
            //    using (MemoryMapped b_mm = new MemoryMapped("R:/dbase/b.dbf.csv"))
            //    {
            //        //MappedCsvReader csvReader = new MappedCsvReader(l_mm);
            //        //(_, l_rows) = csvReader.ReadFile(1);

            //        MappedCsvReader csvReader = new MappedCsvReader(s_mm);
            //        (_, s_rows) = csvReader.ReadFile(1);

            //        csvReader = new MappedCsvReader(b_mm);
            //        (_, b_rows) = csvReader.ReadFile(1);

            //        //Dictionary<ApiNumber, WellL> wellLs = new Dictionary<ApiNumber, WellL>();

            //        //WellL welll;

            //        //foreach (string[] row in l_rows)
            //        //{
            //        //    welll = new WellL(row);

            //        //    if (!wellLs.TryAdd(welll.API, welll))
            //        //    {
            //        //        //Console.WriteLine($"Updating {welll.API}");
            //        //        wellLs[welll.API] = welll;
            //        //    }
            //        //}

            //        Dictionary<long, WellS> wellSs = new Dictionary<long, WellS>();

            //        WellS wellS;

            //        foreach (string[] row in s_rows)
            //        {
            //            wellS = new WellS(row);

            //            if (!wellSs.TryAdd(wellS.SURFACE_ID, wellS))
            //            {
            //                //Console.WriteLine($"Updating {wellS.API}");
            //                wellSs[wellS.SURFACE_ID] = wellS;
            //            }
            //        }

            //        Dictionary<long, WellB> wellBs = new Dictionary<long, WellB>();

            //        WellB wellB;

            //        foreach (string[] row in b_rows)
            //        {
            //            wellB = new WellB(row);

            //            if (!wellBs.TryAdd(wellB.BOTTOM_ID, wellB))
            //            {
            //                //Console.WriteLine($"Updating {wellB.API}");
            //                wellBs[wellB.BOTTOM_ID] = wellB;
            //            }
            //        }

            //        List<ShapeFileLocation> locations = new List<ShapeFileLocation>(wellLs.Count);

            //        foreach (KeyValuePair<ApiNumber, WellL> row in wellLs)
            //        {
            //            if (wellSs.TryGetValue(row.Value.SURFACE_ID, out wellS))
            //            {
            //                locations.Add(wellBs.TryGetValue(row.Value.BOTTOM_ID, out wellB)
            //                                  ? new ShapeFileLocation(row.Key, wellS.LAT83, wellS.LONG83, wellB.LAT83, wellB.LONG83)
            //                                  : new ShapeFileLocation(row.Key, wellS.LAT83, wellS.LONG83, wellS.LAT83, wellS.LONG83));
            //            }
            //        }

            //        RrcTexasDataAdapter adapter = new RrcTexasDataAdapter();
            //        adapter.AddRange(locations);
            //    }
            //}
        }

        public static void FtpDirectory(string url)
        {
            FtpWebRequest listRequest = (FtpWebRequest)WebRequest.Create(url);
            listRequest.Method = WebRequestMethods.Ftp.ListDirectoryDetails;
            //listRequest.Credentials = credentials;

            List<string> lines = new List<string>();

            using(FtpWebResponse listResponse = (FtpWebResponse)listRequest.GetResponse())
            using(Stream listStream = listResponse.GetResponseStream())
            using(StreamReader listReader = new StreamReader(listStream))
            {
                while(!listReader.EndOfStream)
                {
                    lines.Add(listReader.ReadLine());
                }
            }

            foreach(string line in lines)
            {
                string[] tokens = line.Split(new[] {' '}, 9, StringSplitOptions.RemoveEmptyEntries);

                string name        = tokens[8];
                string permissions = tokens[0];

                //string localFilePath = Path.Combine(localPath, name);
                string fileUrl = url + name;

                if(permissions[0] == 'd' && url == "ftp://ftpe.rrc.texas.gov/")
                {
                    try
                    {
                        FtpDirectory(fileUrl + "/");
                    }
                    catch(Exception)
                    {
                        //
                    }
                }
                else
                {
                    Console.WriteLine(fileUrl);
                }
            }
        }

        [DllImport("winmm.dll", CharSet = CharSet.Auto)]
        private static extern bool PlaySound(string lpszName,
                                             IntPtr hModule,
                                             int    dwFlags);

        public static void InterpTest()
        {
            int num_threads = 4;
            int num_numa    = 1;
            int device_id   = 0;
            int ndevices    = 1;
            int skip_device = 9999;

            InitArguments arguments = new InitArguments(num_threads, num_numa, device_id, ndevices, skip_device, false);

            using(ScopeGuard.Get(arguments))
            {
                View<double, Cuda> xd = new View<double, Cuda>("src_lat_long", TestData.lng_lat_oil_api.Length, 2);
                View<double, Cuda> zd = new View<double, Cuda>("oil_api",      TestData.lng_lat_oil_api.Length);

                for(ulong i0 = 0; i0 < xd.Extent(0); ++i0)
                {
                    (double X, double Y) = CoordinateConverter.toWebMercator(TestData.lng_lat_oil_api[i0].lng, TestData.lng_lat_oil_api[i0].lat);
                    xd[i0, 0]            = X;
                    xd[i0, 1]            = Y;
                    zd[i0]               = TestData.lng_lat_oil_api[i0].oil_api;

                    Console.WriteLine($"{xd[i0, 0]} {xd[i0, 1]} {zd[i0]}");
                }

                View<double, Cuda> xi = new View<double, Cuda>("dest_lat_long", TestData.offset_lng_lat.Length, 2);

                for(ulong i0 = 0; i0 < xi.Extent(0); ++i0)
                {
                    (double X, double Y) = CoordinateConverter.toWebMercator(TestData.offset_lng_lat[i0].lng, TestData.offset_lng_lat[i0].lat);
                    xi[i0, 0]            = X;
                    xi[i0, 1]            = Y;
                }

                View<double, Cuda> zi = InterpolationMethods<double, Cuda>.Shepard2d(xd, zd, 2.0, xi);

                for(ulong i0 = 0; i0 < zi.Size(); ++i0)
                {
                    Console.WriteLine($"{xi[i0, 0]} {xi[i0, 1]} {zi[i0]}");
                }
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static void Interp(List<GeoPoint> lng_lat_oil_api,
                                  List<GeoPoint> offset_lng_lat)
        {
            int num_threads = 4;
            int num_numa    = 1;
            int device_id   = 0;
            int ndevices    = 1;
            int skip_device = 9999;

            InitArguments arguments = new InitArguments(num_threads, num_numa, device_id, ndevices, skip_device, false);

            using(ScopeGuard.Get(arguments))
            {
                View<double, Cuda> xd = new View<double, Cuda>("src_lat_long", lng_lat_oil_api.Count, 2);
                View<double, Cuda> zd = new View<double, Cuda>("oil_api",      lng_lat_oil_api.Count);

                for(int i0 = 0; i0 < (int)xd.Extent(0); ++i0)
                {
                    xd[i0, 0] = lng_lat_oil_api[i0].Easting;
                    xd[i0, 1] = lng_lat_oil_api[i0].Northing;
                    zd[i0]    = lng_lat_oil_api[i0].PropertyValue;
                }

                View<double, Cuda> xi = new View<double, Cuda>("dest_lat_long", offset_lng_lat.Count, 2);

                for(int i0 = 0; i0 < (int)xi.Extent(0); ++i0)
                {
                    xi[i0, 0] = offset_lng_lat[i0].Easting;
                    xi[i0, 1] = offset_lng_lat[i0].Northing;
                }

                View<double, Cuda> zi = InterpolationMethods<double, Cuda>.Shepard2d(xd, zd, 2.0, xi);

                for(int i0 = 0; i0 < (int)zi.Size(); ++i0)
                {
                    offset_lng_lat[i0].PropertyValue = zi[i0];
                }
            }
        }

        [STAThread]
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private static void UpdateWells()
        {
            //Console.WriteLine("Building Tables");

            //using(RrcTexasDataAdapter rrc = new RrcTexasDataAdapter())
            //{
            //}

            //try
            //{
            //    Console.WriteLine("LoadTexasDbs");
            //    RrcTexasDataAdapter.LoadTexasDbs(2);
            //}
            //catch(Exception ex)
            //{
            //    Console.WriteLine(ex);
            //}

            ImmutableList<ApiNumber> apis;

            using (RrcTexasDataAdapter rrc = new RrcTexasDataAdapter())
            {
                //rrc.ReIndexDb();

                //rrc.Context.ChangeTracker.AutoDetectChangesEnabled = true;

                apis = rrc.GetAllWells().Select(w => w.Api).ToImmutableList();
            }

            //if(apis is null)
            //{
            //    return;
            //}

            //Console.WriteLine($"Apis Count:{apis.Count}");

            //try
            //{
            //    Console.WriteLine("LoadTexasDbs2");
            //    RrcTexasDataAdapter.LoadTexasDbs2(apis, 2);
            //}
            //catch (Exception ex)
            //{
            //    Console.WriteLine(ex);
            //}

            ////EF.CompileQuery()

            //try
            //{
            //    Console.WriteLine("LoadTexasDbs3");
            //    RrcTexasDataAdapter.LoadTexasDbs3(apis, 2);
            //}
            //catch (Exception ex)
            //{
            //    Console.WriteLine(ex);
            //}

            //try
            //{
            //    Console.WriteLine("LoadTexasDbs4");
            //    RrcTexasDataAdapter.LoadTexasDbs4(apis, 2);
            //}
            //catch(Exception ex)
            //{
            //    Console.WriteLine(ex);
            //}

            //try
            //{
            //    Console.WriteLine("LoadTexasDbs5");

            //    using(TexasAggregateContext tac = new TexasAggregateContext())
            //    {
            //        tac.ChangeTracker.AutoDetectChangesEnabled = false;

            //        ImmutableList<LeaseTestAggr> leaseTests = tac.LeaseTestAggrTable.Where(w => w.DISTRICT_NUMBER == 2).ToImmutableList();

            //        RrcTexasDataAdapter.LoadTexasDbs5(leaseTests);

            //        tac.Connection.Close();
            //    }
            //}
            //catch(Exception ex)
            //{
            //    Console.WriteLine(ex);
            //}

            try
            {
                Console.WriteLine("LoadTexasDbs6");
                RrcTexasDataAdapter.LoadTexasDbs6(apis);
            }
            catch (Exception ex)
            {
                Console.WriteLine(ex);
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private static void InterpWells()
        {
            using(RrcTexasDataAdapter rrc = new RrcTexasDataAdapter())
            {
                rrc.Context.ChangeTracker.AutoDetectChangesEnabled = false;

                ImmutableList<ApiNumber> apis = rrc.GetAllWells().Select(w => w.Api).ToImmutableList();

                if(apis is null)
                {
                    return;
                }

                Console.WriteLine($"Apis Count:{apis.Count}");

                List<GeoPoint> lng_lat_oil_apis = new List<GeoPoint>(100);
                List<GeoPoint> offset_lng_lats  = new List<GeoPoint>(apis.Count);

                Parallel.ForEach(Partitioner.Create(0, apis.Count, apis.Count / Environment.ProcessorCount),
                                 row =>
                                 {
                                     int threadId = System.Threading.Thread.CurrentThread.ManagedThreadId;

                                     using RrcTexasDataAdapter _context = new RrcTexasDataAdapter();

                                     Well well;

                                     for(int i = row.Item1; i < row.Item2; i++)
                                     {
                                         well = _context.GetWellByApi(apis[i]);

                                         if(well.Location != null)
                                         {
                                             if(well.ReservoirData.Count            > 0     &&
                                                well.ReservoirData[0].ReservoirName != null &&
                                                well.ReservoirData[0].ReservoirName.Contains("Eagle", StringComparison.OrdinalIgnoreCase))
                                             {
                                                 if(well.ReservoirData[0].OilProperties?.Density != null)
                                                 {
                                                     lng_lat_oil_apis.Add(new GeoPoint(well.Api,
                                                                                       well.Location!.Easting83!.Value,
                                                                                       well.Location!.Northing83!.Value,
                                                                                       well.ReservoirData[0]!.OilProperties!.Density.Value));
                                                 }
                                                 else
                                                 {
                                                     offset_lng_lats.Add(new GeoPoint(well.Api, well.Location!.Easting83!.Value, well.Location!.Northing83!.Value));
                                                 }
                                             }
                                         }

                                         if(i % 100 == 0)
                                         {
                                             Console.WriteLine($"{threadId}: {i - row.Item1}");
                                         }
                                     }

                                     _context.CloseConnection();
                                 });

                Interp(lng_lat_oil_apis, offset_lng_lats);

                Parallel.ForEach(Partitioner.Create(0, offset_lng_lats.Count, offset_lng_lats.Count / Environment.ProcessorCount),
                                 row =>
                                 {
                                     int threadId = System.Threading.Thread.CurrentThread.ManagedThreadId;

                                     using RrcTexasDataAdapter _context = new RrcTexasDataAdapter();

                                     Well well;

                                     for(int i = row.Item1; i < row.Item2; i++)
                                     {
                                         well = _context.GetWellByApi(apis[i]);

                                         well.ReservoirData[0]!.OilProperties!.Density = offset_lng_lats[i].PropertyValue;

                                         if(i % 100 == 0)
                                         {
                                             Console.WriteLine($"{threadId}: {i - row.Item1}");
                                         }

                                         _context.Update(well);
                                     }

                                     _context.Commit();
                                     _context.CloseConnection();
                                 });
            }
        }

        [STAThread]
        private static void Main(string[] args)
        {

        
            RrcTexasDataAdapter adapter = new RrcTexasDataAdapter();

            Well well = adapter.GetMonthlyProductionFromWebsiteByApi("42-255-35980-00-00").Result;

            foreach (MonthlyProduction monthlyProduction in well.MonthlyProduction)
            {
                Console.WriteLine($"{monthlyProduction.Date}");
            }



            //docker build -t simplestaticblazor . docker run -it --rm -p 5000:80 simplestaticblazor

            //UpdateWells();

            //InterpWells();

            //InterpTest();

            //DbfClassTemplates.Run();

            //Console.WriteLine("LoadWellDbf");
            //using(TexasDumpDbContext tac = new TexasDumpDbContext())
            //{
            //    tac.LoadWellDbf();
            //}

            //Console.WriteLine("BuildLocationTable");

            //using (TexasAggregateContext tac = new TexasAggregateContext())
            //{
            //    //tac.BuildWellTestAggrTable();
            //    //tac.BuildWellProductionAggrTable();
            //    //tac.FixProductionAggrTable();
            //    tac.BuildLeaseTestAggrTable();
            //}

            //using (TexasWellboreContext twc = new TexasWellboreContext())
            //{
            //    //WellBoreTechnicalDataRoot wellBoreTechnicalDataRoot = twc.ByApi("42-123-32754-00-00");

            //    //var wellBoreCompletionInformations = wellBoreTechnicalDataRoot.WellBoreCompletionInformation;

            //    twc.ChangeTracker.AutoDetectChangesEnabled = false;

            //    twc.LoadFieldInformation_dbf900(@"R:\dbf900.ebc.0", @"R:\dbf900.ebc.1", @"R:\dbf900.ebc.2", @"R:\dbf900.ebc.3", @"R:\dbf900.ebc.4");

            //    twc.ChangeTracker.AutoDetectChangesEnabled = true;
            //}

            //using (TexasAggregateContext tac = new TexasAggregateContext())
            //{
            //    tac.BuildWellProductionAggrTable();
            //}

            //FileUtilities.SplitFile(@"D:\OilGasData\RRC\dbf900.ebc", @"D:\OilGasData\RRC\Parts", 247, 132, 4);

            //TexasDumpDbContext.SplitFile(@"T:\flf900.ebc", @"D:\OilGasData\RRC\Parts", 240, 134, 4);
            //TexasDumpWLDbContext.SplitFile(@"T:\wlf100.ebc", @"D:\OilGasData\RRC\Parts", 228, 143, 4);
            //TexasDumpWLDbContext.SplitFile(@"T:\wlf101.ebc", @"D:\OilGasData\RRC\Parts", 228, 143, 4);

            //WLClassTemplates.Run();

            //using (TexasDumpWLDbContext tdc = new TexasDumpWLDbContext())
            //{
            //    tdc.ChangeTracker.AutoDetectChangesEnabled = false;

            //    //tdc.LoadFieldInformation_olf(@"R:\olf001l.ebc");
            //    //tdc.LoadFieldInformation_olf(@"R:\olf003l.ebc");
            //    //tdc.LoadFieldInformation_olf(@"R:\olf004l.ebc");
            //    //tdc.LoadFieldInformation_olf(@"R:\olf005l.ebc");
            //    //tdc.LoadFieldInformation_olf(@"R:\olf007l.ebc");
            //    //tdc.LoadFieldInformation_olf(@"R:\olf008l.ebc");
            //    //tdc.LoadFieldInformation_olf(@"R:\olf009l.ebc");
            //    //tdc.LoadFieldInformation_olf(@"R:\olf010l.ebc");
            //    //tdc.LoadFieldInformation_olf(@"R:\olf011l.ebc");
            //    //tdc.LoadFieldInformation_olf(@"R:\olf013l.ebc");
            //    //tdc.LoadFieldInformation_olf(@"R:\olf014l.ebc");

            //    //tdc.LoadFieldInformation_gsf(@"R:\gsf001l.ebc");
            //    //tdc.LoadFieldInformation_gsf(@"R:\gsf002l.ebc");
            //    //tdc.LoadFieldInformation_gsf(@"R:\gsf003l.ebc");

            //    //tdc.LoadFieldInformation_wlf100(@"R:\wlf100.ebc.0", @"R:\wlf100.ebc.1", @"R:\wlf100.ebc.2", @"R:\wlf100.ebc.3", @"R:\wlf100.ebc.4");
            //    //tdc.LoadFieldInformation_wlf101(@"T:\wlf101.ebc");

            //    tdc.ChangeTracker.AutoDetectChangesEnabled = true;
            //}

            //TexasDumpDbContext.FixQuotes(@$"R:/OG_FIELD_DW_DATA_TABLE.dsv" );

            //TexasDumpDbContext.FixQuotes(@$"R:/OG_OPERATOR_DW_DATA_TABLE.dsv" );

            //TexasDumpDbContext.FixQuotes(@$"R:/OG_REGULATORY_LEASE_DW_DATA_TABLE.dsv" );

            //Console.WriteLine(@$"T:/_OgLeaseCycleData__202005251120.csv");
            //TexasDumpDbContext.RemoveIdColumn(@$"T:/_OgLeaseCycleData__202005251120.csv");

            //Parallel.ForEach(Partitioner.Create(90, 115),
            //                 row =>
            //                 {
            //                     for(int i = row.Item1; i < row.Item2; i++)
            //                     {
            //                         Console.WriteLine(@$"T:/_OgLeaseCycleData__202005251120_{i}.csv");
            //                         TexasDumpDbContext.RemoveIdColumn(@$"T:/_OgLeaseCycleData__202005251120_{i}.csv");
            //                     }
            //                 });

            //for (int i = 7; i < 90; ++i)
            //{
            //    Console.WriteLine(@$"T:/_OgLeaseCycleData__202005251120_{i}.csv");
            //    TexasDumpDbContext.RemoveIdColumn(@$"T:/_OgLeaseCycleData__202005251120_{i}.csv");
            //}

            //using (TexasDumpDbContext tdc = new TexasDumpDbContext())
            //{

            //    tdc.ChangeTracker.AutoDetectChangesEnabled = false;

            //    //tdc.LoadOgRegulatoryLeaseDwDsv(@"R:/OG_REGULATORY_LEASE_DW_DATA_TABLE.dsv");
            //    tdc.LoadOgFieldDwDsv(@"R:/OG_FIELD_DW_DATA_TABLE.dsv");
            //    //tdc.LoadOgOperatorDwDsv(@"R:/OG_OPERATOR_DW_DATA_TABLE.dsv");

            //    //tdc.LoadOgFieldCycleDsv(@"R:/OG_FIELD_CYCLE_DATA_TABLE.dsv");
            //    //tdc.LoadOgWellCompletionDataDsv(@"R:/OG_WELL_COMPLETION_DATA_TABLE.dsv");

            //    //Console.WriteLine(@"T:/OG_LEASE_CYCLE_DATA_TABLE.dsv.0");tdc.LoadOgLeaseCycleDataDsv(@"T:/OG_LEASE_CYCLE_DATA_TABLE.dsv.0", 1);
            //    //Console.WriteLine(@"T:/OG_LEASE_CYCLE_DATA_TABLE.dsv.1");tdc.LoadOgLeaseCycleDataDsv(@"T:/OG_LEASE_CYCLE_DATA_TABLE.dsv.1", 0);
            //    //4343241

            //    //for (int i = 1; i < 40; ++i)
            //    //{
            //    //    Console.WriteLine(@$"T:/OG_LEASE_CYCLE_DATA_TABLE.dsv.{i}");
            //    //    //tdc.LoadOgLeaseCycleDataDsv(@$"T:/OG_LEASE_CYCLE_DATA_TABLE.dsv.{i}", 0);
            //    //    TexasDumpDbContext.FixLineEnding(@$"T:/OG_LEASE_CYCLE_DATA_TABLE.dsv.{i}");
            //    //    //tdc.CopyFileToDb("OgLeaseCycleData", OG_LEASE_CYCLE_DATA_HEADER, @$"T:/OG_LEASE_CYCLE_DATA_TABLE.dsv.{i}", "}", false);
            //    //    //TexasDumpDbContext.FixQuotes(@$"T:/OG_LEASE_CYCLE_DATA_TABLE.dsv.{i}");
            //    //}

            //    //Console.WriteLine(@"R:\flf900.ebc.0"); tdc.LoadFieldInformation_flf900(@"T:\flf900.ebc.0");
            //    //Console.WriteLine(@"R:\flf900.ebc.1"); tdc.LoadFieldInformation_flf900(@"T:\flf900.ebc.1");
            //    //Console.WriteLine(@"R:\flf900.ebc.2"); tdc.LoadFieldInformation_flf900(@"T:\flf900.ebc.2");
            //    //Console.WriteLine(@"R:\flf900.ebc.3"); tdc.LoadFieldInformation_flf900(@"T:\flf900.ebc.3");
            //    ////tdc.LoadFieldInformation_flf900(@"R:\flf900.ebc.4");

            //    tdc.ChangeTracker.AutoDetectChangesEnabled = true;
            //}

            //TexasDumpDbContext.SplitFileByLineEnding(@"T:\OG_LEASE_CYCLE_DATA_TABLE.dsv", @"D:\OilGasData\RRC\Parts", 50);

            //TexasDumpDbContext.SplitFile(@"T:\flf900.ebc", @"D:\OilGasData\RRC\Parts", 240, 134, 4);

            //FtpDirectory("ftp://ftpe.rrc.texas.gov/");

            //RrcTexasDataAdapter adapter = new RrcTexasDataAdapter();

            //adapter.ConvertLocations();

            //ConvertToUtf8(@"R:/flf900.ebc", @"T:/flf900.txt");

            //List<Well> wells = adapter.GetAllWellsIncluding().ToList();

            //List<Well> wellsByApi;
            //Well wellByApi;
            //foreach(Well well in wells)
            //{
            //    wellsByApi = adapter.GetWellsByApi(well.Api).ToList();

            //}

            //List<Well> wells = (await adapter.GetWellsByCountyAsync("Karnes")).Where(w => w.CompletionDetails.LateralLength != null).Where(w => w.MonthlyProduction.Count == 0).ToList();

            //foreach(Well well in wells)
            //{
            //    if(well.Api == "42-255-35980-00-00")
            //    {
            //        continue;
            //    }

            //    Console.WriteLine($"{well.Api}");
            //    await adapter.GetMonthlyProductionByApi(well.Api);
            //}

            //await Task.FromResult(Task.CompletedTask);

            //for(int i = 0; i < dt.Rows.Count; i++)
            //{
            //    for(int j = 0; j < dt.Columns.Count; j++)
            //    {
            //        Console.WriteLine(dt.Rows[i][j]);
            //    }
            //}

            //adapter.LoadOperatorsCsv(@"R:\TexasOperators.csv");

            //adapter.LoadDrillingPermitsCsv(@"R:\DrillingPermits_STX_2000_2020.csv");

            //adapter.Backup();

            //await adapter.UpdateAllWellsLocationsAsync();

            //await adapter.UpdateAllWellsMonthlyProductionAsync();

            //TestPvt();

            //await adapter.UpdateAllWellsHydrocarbonPropertiesAsync();

            //(string csvData, Lease lease) = QueryBuilder.SpecificLeaseProductionQueryByApi("4225532662").Result;

            //@"C:\Users\tehgo\Desktop\viewPdfReportFormAction.pdf"

            //ReadPdfs();

            //GetLocation(Well well)

            //DbFixes();

            //QueryProductionByApi();

            // DirectionalSurvey ds = QueryBuilder.CompletionReportQueryByApi("4225532662").Result;
            //
            // Console.WriteLine($"From: {ds.From} To:{ds.To}");

            //GetWellLocations();

            //LoadCsvData();

            //IEnumerable<string> organizationNames = QueryBuilder.OrganizationNameQueryByNumber("947128");

            //foreach (string organizationName in organizationNames)
            //{
            //    Console.WriteLine(organizationName);
            //}

            //TestProductionChartCollection();

            //Test5();

            //TestDb0();

            //Test5();

            //TestDb1();
            //TestDb2();

            PlaySound(@"C:\Windows\Media\tada.wav", IntPtr.Zero, 0);

#if DEBUG
            Console.WriteLine("press any key to exit.");
            Console.ReadKey();
#endif
        }

        private static void TestProductionChartCollection()
        {
            //ProductionChartCollection vegaViewData = new ProductionChartCollection();

            //#region Values

            //vegaViewData.Add("Actual", 1.0, 280.51724137931035);

            //vegaViewData.Add("Actual", 2.0, 691.5172413793103);

            //vegaViewData.Add("Actual", 3.0, 1336.5172413793102);

            //vegaViewData.Add("Actual", 4.0, 1391.4172413793103);

            //vegaViewData.Add("Actual", 5.0, 1316.2172413793105);

            //vegaViewData.Add("Actual", 6.0, 1275.3872413793101);

            //vegaViewData.Add("Actual", 7.0, 1339.7672413793102);

            //vegaViewData.Add("Actual", 8.0, 1715.2865517241378);

            //vegaViewData.Add("Actual", 9.0, 1995.65);

            //vegaViewData.Add("Actual", 10.0, 1757.1662068965516);

            //vegaViewData.Add("Actual", 11.0, 1714.0844827586207);

            //vegaViewData.Add("Actual", 12.0, 1490.5651724137929);

            //vegaViewData.Add("Actual", 13.0, 1543.6537931034484);

            //vegaViewData.Add("Actual", 14.0, 1581.0941379310343);

            //vegaViewData.Add("Actual", 15.0, 1588.4506896551723);

            //vegaViewData.Add("Actual", 16.0, 1499.3837931034484);

            //vegaViewData.Add("Actual", 17.0, 1553.005172413793);

            //vegaViewData.Add("Actual", 18.0, 1529.0706896551724);

            //vegaViewData.Add("Actual", 19.0, 1405.986551724138);

            //vegaViewData.Add("Actual", 20.0, 1363.6072413793104);

            //vegaViewData.Add("Actual", 21.0, 1262.5103448275863);

            //vegaViewData.Add("Actual", 22.0, 1224.0679310344826);

            //vegaViewData.Add("Actual", 23.0, 1225.2134482758622);

            //vegaViewData.Add("Actual", 24.0, 1171.863448275862);

            //vegaViewData.Add("Actual", 25.0, 1152.3672413793101);

            //vegaViewData.Add("Actual", 26.0, 1093.0206896551724);

            //vegaViewData.Add("Actual", 27.0, 1093.871379310345);

            //vegaViewData.Add("Actual", 28.0, 1124.1062068965516);

            //vegaViewData.Add("Actual", 29.0, 1064.4989655172412);

            //vegaViewData.Add("Actual", 30.0, 1001.3689655172413);

            //vegaViewData.Add("Actual", 31.0, 964.7772413793103);

            //vegaViewData.Add("Actual", 32.0, 939.2848275862069);

            //vegaViewData.Add("Actual", 33.0, 925.7072413793104);

            //vegaViewData.Add("Actual", 34.0, 964.56);

            //vegaViewData.Add("Actual", 35.0, 900.3720689655172);

            //vegaViewData.Add("Actual", 36.0, 963.2882758620689);

            //vegaViewData.Add("Actual", 37.0, 904.4734482758621);

            //vegaViewData.Add("Actual", 38.0, 903.9796551724138);

            //vegaViewData.Add("Actual", 39.0, 875.2172413793104);

            //vegaViewData.Add("Actual", 40.0, 807.8434482758621);

            //vegaViewData.Add("Actual", 41.0, 873.9244827586207);

            //vegaViewData.Add("Actual", 42.0, 870.831724137931);

            //vegaViewData.Add("Actual", 43.0, 797.2489655172415);

            //vegaViewData.Add("Actual", 44.0, 787.8537931034483);

            //vegaViewData.Add("Actual", 45.0, 752.8534482758621);

            //vegaViewData.Add("Actual", 46.0, 647.0982758620689);

            //vegaViewData.Add("Actual", 47.0, 814.1565517241379);

            //vegaViewData.Add("Actual", 48.0, 664.97);

            //vegaViewData.Add("Actual", 49.0, 685.3151724137932);

            //vegaViewData.Add("Actual", 50.0, 743.7024137931035);

            //vegaViewData.Add("Actual", 51.0, 722.8972413793103);

            //vegaViewData.Add("Actual", 52.0, 743.291724137931);

            //vegaViewData.Add("Actual", 53.0, 683.1786206896552);

            //vegaViewData.Add("Actual", 54.0, 635.5079310344828);

            //vegaViewData.Add("Actual", 55.0, 629.658275862069);

            //vegaViewData.Add("Actual", 56.0, 640.6393103448275);

            //vegaViewData.Add("Actual", 57.0, 639.9817241379311);

            //vegaViewData.Add("Actual", 58.0, 645.3796551724139);

            //vegaViewData.Add("Actual", 59.0, 671.9613793103449);

            //vegaViewData.Add("Actual", 60.0, 722.2475862068966);

            //vegaViewData.Add("Actual", 61.0, 667.328620689655);

            //vegaViewData.Add("Actual", 62.0, 615.588620689655);

            //vegaViewData.Add("Actual", 63.0, 648.4493103448275);

            //vegaViewData.Add("Actual", 64.0, 620.6334482758621);

            //vegaViewData.Add("Actual", 65.0, 640.9768965517242);

            //vegaViewData.Add("Actual", 66.0, 532.3906896551724);

            //vegaViewData.Add("Actual", 67.0, 588.7486206896551);

            //vegaViewData.Add("Actual", 68.0, 673.6427586206897);

            //vegaViewData.Add("Actual", 69.0, 566.2744827586207);

            //vegaViewData.Add("Actual", 70.0, 552.903448275862);

            //vegaViewData.Add("Actual", 71.0, 502.15862068965515);

            //vegaViewData.Add("Actual", 72.0, 508.87827586206896);

            //vegaViewData.Add("Actual", 73.0, 462.50275862068963);

            //vegaViewData.Add("Actual", 74.0, 455.1955172413793);

            //vegaViewData.Add("Actual", 75.0, 416.2444827586207);

            //vegaViewData.Add("Actual", 76.0, 443.56172413793104);

            //vegaViewData.Add("Actual", 77.0, 457.7024137931034);

            //vegaViewData.Add("Actual", 78.0, 447.36413793103446);

            //vegaViewData.Add("Actual", 79.0, 433.50689655172414);

            //vegaViewData.Add("Actual", 80.0, 452.21172413793107);

            //vegaViewData.Add("Actual", 81.0, 441.0144827586207);

            //vegaViewData.Add("Actual", 82.0, 438.1520689655173);

            //vegaViewData.Add("Actual", 83.0, 432.46724137931034);

            //vegaViewData.Add("Actual", 84.0, 621.1710344827586);

            //vegaViewData.Add("Actual", 85.0, 658.9341379310346);

            //vegaViewData.Add("Actual", 86.0, 607.4920689655172);

            //vegaViewData.Add("Actual", 87.0, 602.9531034482759);

            //vegaViewData.Add("Actual", 88.0, 562.7348275862068);

            //vegaViewData.Add("Actual", 89.0, 571.6103448275861);

            //vegaViewData.Add("Actual", 90.0, 521.5062068965517);

            //vegaViewData.Add("Actual", 91.0, 489.8824137931034);

            //vegaViewData.Add("Actual", 92.0, 497.07482758620694);

            //vegaViewData.Add("Actual", 93.0, 481.1875862068966);

            //vegaViewData.Add("Actual", 94.0, 491.22275862068966);

            //vegaViewData.Add("Actual", 95.0, 458.9603448275862);

            //vegaViewData.Add("Actual", 96.0, 476.9755172413793);

            //vegaViewData.Add("Actual", 97.0, 461.89620689655175);

            //vegaViewData.Add("Actual", 98.0, 446.93379310344824);

            //vegaViewData.Add("Actual", 99.0, 451.11655172413793);

            //vegaViewData.Add("Actual", 100.0, 444.2193103448276);

            //vegaViewData.Add("Actual", 101.0, 469.00931034482755);

            //vegaViewData.Add("Actual", 102.0, 435.2944827586207);

            //vegaViewData.Add("Actual", 103.0, 451.0420689655173);

            //vegaViewData.Add("Actual", 104.0, 436.62793103448274);

            //vegaViewData.Add("Actual", 105.0, 419.5458620689655);

            //vegaViewData.Add("Actual", 106.0, 434.31482758620695);

            //vegaViewData.Add("Actual", 107.0, 413.47);

            //vegaViewData.Add("Actual", 108.0, 416.48517241379307);

            //vegaViewData.Add("Actual", 109.0, 417.7606896551724);

            //vegaViewData.Add("Actual", 110.0, 400.49586206896555);

            //vegaViewData.Add("Actual", 111.0, 407.2134482758621);

            //vegaViewData.Add("Actual", 112.0, 407.2334482758621);

            //vegaViewData.Add("Actual", 113.0, 431.0686206896552);

            //vegaViewData.Add("Actual", 114.0, 396.6813793103448);

            //vegaViewData.Add("Actual", 115.0, 404.1189655172414);

            //vegaViewData.Add("Actual", 116.0, 422.84896551724137);

            //vegaViewData.Add("Actual", 117.0, 395.75931034482755);

            //vegaViewData.Add("Actual", 118.0, 398.11931034482757);

            //vegaViewData.Add("Actual", 119.0, 380.37448275862073);

            //vegaViewData.Add("Actual", 120.0, 380.57965517241377);

            //vegaViewData.Add("Actual", 121.0, 382.74724137931037);

            //vegaViewData.Add("Actual", 122.0, 382.87);

            //vegaViewData.Add("Actual", 123.0, 378.1875862068966);

            //vegaViewData.Add("Actual", 124.0, 376.3451724137931);

            //vegaViewData.Add("Actual", 125.0, 383.9851724137931);

            //vegaViewData.Add("Actual", 126.0, 423.42137931034483);

            //vegaViewData.Add("Actual", 127.0, 415.1786206896552);

            //vegaViewData.Add("Actual", 128.0, 399.13413793103445);

            //vegaViewData.Add("Actual", 129.0, 438.10068965517246);

            //vegaViewData.Add("Actual", 130.0, 448.44586206896554);

            //vegaViewData.Add("Actual", 131.0, 409.1089655172414);

            //vegaViewData.Add("Simulated", 1.0, 3385.653031759629);

            //vegaViewData.Add("Simulated", 2.0, 2860.142695744137);

            //vegaViewData.Add("Simulated", 3.0, 2589.8935768088877);

            //vegaViewData.Add("Simulated", 4.0, 2410.1319948087544);

            //vegaViewData.Add("Simulated", 5.0, 2272.281066193681);

            //vegaViewData.Add("Simulated", 6.0, 2158.9606020927845);

            //vegaViewData.Add("Simulated", 7.0, 2058.099696923838);

            //vegaViewData.Add("Simulated", 8.0, 1967.3263832210368);

            //vegaViewData.Add("Simulated", 9.0, 1888.7559318476053);

            //vegaViewData.Add("Simulated", 10.0, 1812.8542098290668);

            //vegaViewData.Add("Simulated", 11.0, 1741.0646877304623);

            //vegaViewData.Add("Simulated", 12.0, 1675.8600631031659);

            //vegaViewData.Add("Simulated", 13.0, 1616.7913313085971);

            //vegaViewData.Add("Simulated", 14.0, 1562.9005952937473);

            //vegaViewData.Add("Simulated", 15.0, 1512.9861375432945);

            //vegaViewData.Add("Simulated", 16.0, 1465.5909241922395);

            //vegaViewData.Add("Simulated", 17.0, 1420.2454569752342);

            //vegaViewData.Add("Simulated", 18.0, 1376.9897544488738);

            //vegaViewData.Add("Simulated", 19.0, 1335.6875044498745);

            //vegaViewData.Add("Simulated", 20.0, 1296.4569820854904);

            //vegaViewData.Add("Simulated", 21.0, 1259.7660978603387);

            //vegaViewData.Add("Simulated", 22.0, 1225.8312256311858);

            //vegaViewData.Add("Simulated", 23.0, 1194.5824949972155);

            //vegaViewData.Add("Simulated", 24.0, 1165.858145883228);

            //vegaViewData.Add("Simulated", 25.0, 1139.4497227645202);

            //vegaViewData.Add("Simulated", 26.0, 1115.0873127871596);

            //vegaViewData.Add("Simulated", 27.0, 1092.451876576308);

            //vegaViewData.Add("Simulated", 28.0, 1071.176627515245);

            //vegaViewData.Add("Simulated", 29.0, 1050.871038256046);

            //vegaViewData.Add("Simulated", 30.0, 1031.168864720826);

            //vegaViewData.Add("Simulated", 31.0, 1011.8110288266415);

            //vegaViewData.Add("Simulated", 32.0, 992.749498237783);

            //vegaViewData.Add("Simulated", 33.0, 974.0936280546399);

            //vegaViewData.Add("Simulated", 34.0, 955.9626756453326);

            //vegaViewData.Add("Simulated", 35.0, 938.4214274539693);

            //vegaViewData.Add("Simulated", 36.0, 921.4897698263893);

            //vegaViewData.Add("Simulated", 37.0, 905.1652347629425);

            //vegaViewData.Add("Simulated", 38.0, 889.434743692923);

            //vegaViewData.Add("Simulated", 39.0, 874.2823897037176);

            //vegaViewData.Add("Simulated", 40.0, 859.692247656633);

            //vegaViewData.Add("Simulated", 41.0, 845.6459278296646);

            //vegaViewData.Add("Simulated", 42.0, 832.1250771900033);

            //vegaViewData.Add("Simulated", 43.0, 819.1105451802897);

            //vegaViewData.Add("Simulated", 44.0, 806.5810226860616);

            //vegaViewData.Add("Simulated", 45.0, 794.514475527713);

            //vegaViewData.Add("Simulated", 46.0, 782.8889550036005);

            //vegaViewData.Add("Simulated", 47.0, 771.6817018708116);

            //vegaViewData.Add("Simulated", 48.0, 760.8693134596393);

            //vegaViewData.Add("Simulated", 49.0, 750.4284750935386);

            //vegaViewData.Add("Simulated", 50.0, 740.3382319647167);

            //vegaViewData.Add("Simulated", 51.0, 730.5837281701004);

            //vegaViewData.Add("Simulated", 52.0, 721.1552109326782);

            //vegaViewData.Add("Simulated", 53.0, 712.0414957052374);

            //vegaViewData.Add("Simulated", 54.0, 703.2274289878537);

            //vegaViewData.Add("Simulated", 55.0, 694.6951597818771);

            //vegaViewData.Add("Simulated", 56.0, 686.424985369958);

            //vegaViewData.Add("Simulated", 57.0, 678.3947494874112);

            //vegaViewData.Add("Simulated", 58.0, 670.579468663986);

            //vegaViewData.Add("Simulated", 59.0, 662.9535447293721);

            //vegaViewData.Add("Simulated", 60.0, 655.4943215059045);

            //vegaViewData.Add("Simulated", 61.0, 648.1868637792459);

            //vegaViewData.Add("Simulated", 62.0, 641.0282347910644);

            //vegaViewData.Add("Simulated", 63.0, 634.025784190688);

            //vegaViewData.Add("Simulated", 64.0, 627.1903296684648);

            //vegaViewData.Add("Simulated", 65.0, 620.5308354373752);

            //vegaViewData.Add("Simulated", 66.0, 614.0527978087471);

            //vegaViewData.Add("Simulated", 67.0, 607.7580336491902);

            //vegaViewData.Add("Simulated", 68.0, 601.6446298322244);

            //vegaViewData.Add("Simulated", 69.0, 595.7073754059601);

            //vegaViewData.Add("Simulated", 70.0, 589.9387326022625);

            //vegaViewData.Add("Simulated", 71.0, 584.3299296285528);

            //vegaViewData.Add("Simulated", 72.0, 578.8714225079025);

            //vegaViewData.Add("Simulated", 73.0, 573.5522707101682);

            //vegaViewData.Add("Simulated", 74.0, 568.359113477507);

            //vegaViewData.Add("Simulated", 75.0, 563.275977800231);

            //vegaViewData.Add("Simulated", 76.0, 558.2855119838997);

            //vegaViewData.Add("Simulated", 77.0, 553.3711313135605);

            //vegaViewData.Add("Simulated", 78.0, 548.5187627466465);

            //vegaViewData.Add("Simulated", 79.0, 543.7172211616356);

            //vegaViewData.Add("Simulated", 80.0, 538.9573867326462);

            //vegaViewData.Add("Simulated", 81.0, 534.2311133394368);

            //vegaViewData.Add("Simulated", 82.0, 529.530677550363);

            //vegaViewData.Add("Simulated", 83.0, 524.848934531839);

            //vegaViewData.Add("Simulated", 84.0, 520.1799526657965);

            //vegaViewData.Add("Simulated", 85.0, 515.5197721112074);

            //vegaViewData.Add("Simulated", 86.0, 510.8668373359399);

            //vegaViewData.Add("Simulated", 87.0, 506.2218817474353);

            //vegaViewData.Add("Simulated", 88.0, 501.5873892238552);

            //vegaViewData.Add("Simulated", 89.0, 496.9668528861871);

            //vegaViewData.Add("Simulated", 90.0, 492.3640330015441);

            //vegaViewData.Add("Simulated", 91.0, 487.78240484972196);

            //vegaViewData.Add("Simulated", 92.0, 483.22487901014574);

            //vegaViewData.Add("Simulated", 93.0, 478.6937700299449);

            //vegaViewData.Add("Simulated", 94.0, 474.1909118984074);

            //vegaViewData.Add("Simulated", 95.0, 469.7177880082405);

            //vegaViewData.Add("Simulated", 96.0, 465.27563401601617);

            //vegaViewData.Add("Simulated", 97.0, 460.86554961992226);

            //vegaViewData.Add("Simulated", 98.0, 456.48862821340566);

            //vegaViewData.Add("Simulated", 99.0, 452.1460364158983);

            //vegaViewData.Add("Simulated", 100.0, 447.8389442429914);

            //vegaViewData.Add("Simulated", 101.0, 443.56831413587776);

            //vegaViewData.Add("Simulated", 102.0, 439.3347030942092);

            //vegaViewData.Add("Simulated", 103.0, 435.1382195712372);

            //vegaViewData.Add("Simulated", 104.0, 430.97864050351023);

            //vegaViewData.Add("Simulated", 105.0, 426.8556030083277);

            //vegaViewData.Add("Simulated", 106.0, 422.7687866738485);

            //vegaViewData.Add("Simulated", 107.0, 418.7180438099082);

            //vegaViewData.Add("Simulated", 108.0, 414.70347444550964);

            //vegaViewData.Add("Simulated", 109.0, 410.7254541948499);

            //vegaViewData.Add("Simulated", 110.0, 406.7846054797985);

            //vegaViewData.Add("Simulated", 111.0, 402.88170606104785);

            //vegaViewData.Add("Simulated", 112.0, 399.01757967829565);

            //vegaViewData.Add("Simulated", 113.0, 395.1930402520451);

            //vegaViewData.Add("Simulated", 114.0, 391.40890502378994);

            //vegaViewData.Add("Simulated", 115.0, 387.66601379634034);

            //vegaViewData.Add("Simulated", 116.0, 383.9651760068833);

            //vegaViewData.Add("Simulated", 117.0, 380.3070253962728);

            //vegaViewData.Add("Simulated", 118.0, 376.69184439895514);

            //vegaViewData.Add("Simulated", 119.0, 373.11945249512513);

            //vegaViewData.Add("Simulated", 120.0, 369.5892065384409);

            //vegaViewData.Add("Simulated", 121.0, 366.1000908610182);

            //vegaViewData.Add("Simulated", 122.0, 362.6508441513587);

            //vegaViewData.Add("Simulated", 123.0, 359.24008340236287);

            //vegaViewData.Add("Simulated", 124.0, 355.86640710268074);

            //vegaViewData.Add("Simulated", 125.0, 352.5284655352057);

            //vegaViewData.Add("Simulated", 126.0, 349.2249830279593);

            //vegaViewData.Add("Simulated", 127.0, 345.95472643069775);

            //vegaViewData.Add("Simulated", 128.0, 342.71644161092155);

            //vegaViewData.Add("Simulated", 129.0, 339.5088033133075);

            //vegaViewData.Add("Simulated", 130.0, 336.33041659109745);

            //vegaViewData.Add("Simulated", 131.0, 333.1798739853478);

            //#endregion

            //vegaViewData.BuildCumulativeProduction(8);

            //foreach(var data in vegaViewData["Actual"])
            //{
            //    Console.WriteLine(data);
            //}

            //vegaViewData.ToMonthlyProduction(new System.DateTime(2012, 10, 15), 15);

            //vegaViewData.BuildCumulativeProduction(8);

            //foreach(var data in vegaViewData["Actual"])
            //{
            //    Console.WriteLine(data);
            //}
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
            // DataStorage ds = new DataStorage("ConcurrentDictionary.Test");
            //
            // Database<int, double[]> db = new Database<int, double[]>(ds);
            //
            // db.KeyValues.TryAdd(0,
            //                     new double[]
            //                     {
            //                         0.2003
            //                     });
            //
            // db.KeyValues.TryAdd(1,
            //                     new double[]
            //                     {
            //                         1.2003
            //                     });
            //
            // db.KeyValues.TryAdd(2,
            //                     new double[]
            //                     {
            //                         2.2003
            //                     });
            //
            // db.Save();
            //
            // Database<int, double[]> loaded_db = Database<int, double[]>.Load(ds);
            //
            // Console.WriteLine(loaded_db.KeyValues[0][0]);
        }

        private static void TestDb2()
        {
            // FracFocusDataAdapter.Initialize(new DataStorage("Rrc.Texas.db"));
            //
            // Registry entry = FracFocusDataAdapter.GetWellByApi("42-317-39174");
            //
            // //foreach(KeyValuePair<Guid, Registry> entry in entries)
            // //{
            // Console.WriteLine(entry);
            // //}
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
            // ApiNumber api = "42-285-33615";
            //
            // RrcTexasDataAdapter.Initialize(new DataStorage("Rrc.Texas.db"));
            //
            // WellProduction wellProduction = RrcTexasDataAdapter.GetProductionByApi(api).Result;
            //
            // RrcTexasDataAdapter.Commit();
            //
            // WellProductionRecord[] production4228533615 = wellProduction.Records.ToArray();
            //
            // Specification specification = WellProduction.DefaultSpecification(nameof(production4228533615));
            //
            // Chart chart = new Chart("Monthly Production", specification, 750, 500);
            //
            // chart.ShowInBrowser();
            //
            // //Console.WriteLine(chart.ToString());
            // //Console.ReadKey();
        }
    }
}
