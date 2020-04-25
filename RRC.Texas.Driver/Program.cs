using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading;

using OilGas.Data;
using OilGas.Data.Charting;
using OilGas.Data.FracFocus;
using OilGas.Data.RRC.Texas;

using VegaLite;
using VegaLite.Schema; //using OilGas.Data.RRC.Texas;

namespace RRC.Texas.Driver
{
    internal class Program
    {
        internal static readonly string BoolName = typeof(bool).Name;


        private static void QueryDrillingPermitsByCounty()
        {
            CountyType.Kind[] counties = new[]
            {
                CountyType.Kind.ATASCOSA,
                CountyType.Kind.DE_WITT,
                CountyType.Kind.LA_SALLE,
                CountyType.Kind.DUVAL,
                CountyType.Kind.LIVE_OAK,
                CountyType.Kind.KARNES,
                CountyType.Kind.FRIO,
                CountyType.Kind.MCMULLEN,
                CountyType.Kind.WEBB,
                CountyType.Kind.ZAVALA,
                CountyType.Kind.MAVERICK
            };

            foreach (var county in counties)
            {
                CountyType countyType = new CountyType(county);

                string data = QueryBuilder.DrillingPermitsQueryByCounty(countyType, System.DateTime.Parse("01/01/2000"), System.DateTime.Parse("01/01/2020")).Result;

                File.WriteAllText($@"R:\DrillingPermits_{countyType}.csv", data);
            }
        }

        private static void Main(string[] args)
        {
            {
                //RrcTexasContext context = new RrcTexasContext();

                //context.Backup();
            }


            {
                //RrcTexasContext context = new RrcTexasContext();

                //context.LoadDrillingPermitsCsv(@"C:\Users\tehgo\Desktop\DrillingPermits_STX_2000_2020.csv");

                //context.Backup();
            }


            {
                RrcTexasContext context = new RrcTexasContext();

                context.LoadDb(@"D:\TFS_Sources\Github\Compilation\trmcnealy\OilGas.Data\RRC.Texas.Driver\bin\Debug\netcoreapp3.1\Rrc.Texas.db");

                List<DrillingPermit> drillingPermits = context.DrillingPermits.Where(dp => dp.WellboreProfile.Contains("Horizontal")).ToList();

                foreach (DrillingPermit drillingPermit in drillingPermits)
                {
                    try
                    {
                        WellProduction production = context.GetProductionByApi(drillingPermit.Api).Result;
                        Console.WriteLine(production.Api);
                    }
                    catch(Exception e)
                    {
                        Console.WriteLine(e);
                    }

                    Thread.Sleep(5000);
                }

                context.Backup();
            }




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

#if DEBUG
            Console.WriteLine("press any key to exit.");
            Console.ReadKey();
#endif
        }

        private static void TestProductionChartCollection()
        {
            ProductionChartCollection vegaViewData = new ProductionChartCollection();

            #region Values

            vegaViewData.Add("Actual", 1.0, 280.51724137931035);

            vegaViewData.Add("Actual", 2.0, 691.5172413793103);

            vegaViewData.Add("Actual", 3.0, 1336.5172413793102);

            vegaViewData.Add("Actual", 4.0, 1391.4172413793103);

            vegaViewData.Add("Actual", 5.0, 1316.2172413793105);

            vegaViewData.Add("Actual", 6.0, 1275.3872413793101);

            vegaViewData.Add("Actual", 7.0, 1339.7672413793102);

            vegaViewData.Add("Actual", 8.0, 1715.2865517241378);

            vegaViewData.Add("Actual", 9.0, 1995.65);

            vegaViewData.Add("Actual", 10.0, 1757.1662068965516);

            vegaViewData.Add("Actual", 11.0, 1714.0844827586207);

            vegaViewData.Add("Actual", 12.0, 1490.5651724137929);

            vegaViewData.Add("Actual", 13.0, 1543.6537931034484);

            vegaViewData.Add("Actual", 14.0, 1581.0941379310343);

            vegaViewData.Add("Actual", 15.0, 1588.4506896551723);

            vegaViewData.Add("Actual", 16.0, 1499.3837931034484);

            vegaViewData.Add("Actual", 17.0, 1553.005172413793);

            vegaViewData.Add("Actual", 18.0, 1529.0706896551724);

            vegaViewData.Add("Actual", 19.0, 1405.986551724138);

            vegaViewData.Add("Actual", 20.0, 1363.6072413793104);

            vegaViewData.Add("Actual", 21.0, 1262.5103448275863);

            vegaViewData.Add("Actual", 22.0, 1224.0679310344826);

            vegaViewData.Add("Actual", 23.0, 1225.2134482758622);

            vegaViewData.Add("Actual", 24.0, 1171.863448275862);

            vegaViewData.Add("Actual", 25.0, 1152.3672413793101);

            vegaViewData.Add("Actual", 26.0, 1093.0206896551724);

            vegaViewData.Add("Actual", 27.0, 1093.871379310345);

            vegaViewData.Add("Actual", 28.0, 1124.1062068965516);

            vegaViewData.Add("Actual", 29.0, 1064.4989655172412);

            vegaViewData.Add("Actual", 30.0, 1001.3689655172413);

            vegaViewData.Add("Actual", 31.0, 964.7772413793103);

            vegaViewData.Add("Actual", 32.0, 939.2848275862069);

            vegaViewData.Add("Actual", 33.0, 925.7072413793104);

            vegaViewData.Add("Actual", 34.0, 964.56);

            vegaViewData.Add("Actual", 35.0, 900.3720689655172);

            vegaViewData.Add("Actual", 36.0, 963.2882758620689);

            vegaViewData.Add("Actual", 37.0, 904.4734482758621);

            vegaViewData.Add("Actual", 38.0, 903.9796551724138);

            vegaViewData.Add("Actual", 39.0, 875.2172413793104);

            vegaViewData.Add("Actual", 40.0, 807.8434482758621);

            vegaViewData.Add("Actual", 41.0, 873.9244827586207);

            vegaViewData.Add("Actual", 42.0, 870.831724137931);

            vegaViewData.Add("Actual", 43.0, 797.2489655172415);

            vegaViewData.Add("Actual", 44.0, 787.8537931034483);

            vegaViewData.Add("Actual", 45.0, 752.8534482758621);

            vegaViewData.Add("Actual", 46.0, 647.0982758620689);

            vegaViewData.Add("Actual", 47.0, 814.1565517241379);

            vegaViewData.Add("Actual", 48.0, 664.97);

            vegaViewData.Add("Actual", 49.0, 685.3151724137932);

            vegaViewData.Add("Actual", 50.0, 743.7024137931035);

            vegaViewData.Add("Actual", 51.0, 722.8972413793103);

            vegaViewData.Add("Actual", 52.0, 743.291724137931);

            vegaViewData.Add("Actual", 53.0, 683.1786206896552);

            vegaViewData.Add("Actual", 54.0, 635.5079310344828);

            vegaViewData.Add("Actual", 55.0, 629.658275862069);

            vegaViewData.Add("Actual", 56.0, 640.6393103448275);

            vegaViewData.Add("Actual", 57.0, 639.9817241379311);

            vegaViewData.Add("Actual", 58.0, 645.3796551724139);

            vegaViewData.Add("Actual", 59.0, 671.9613793103449);

            vegaViewData.Add("Actual", 60.0, 722.2475862068966);

            vegaViewData.Add("Actual", 61.0, 667.328620689655);

            vegaViewData.Add("Actual", 62.0, 615.588620689655);

            vegaViewData.Add("Actual", 63.0, 648.4493103448275);

            vegaViewData.Add("Actual", 64.0, 620.6334482758621);

            vegaViewData.Add("Actual", 65.0, 640.9768965517242);

            vegaViewData.Add("Actual", 66.0, 532.3906896551724);

            vegaViewData.Add("Actual", 67.0, 588.7486206896551);

            vegaViewData.Add("Actual", 68.0, 673.6427586206897);

            vegaViewData.Add("Actual", 69.0, 566.2744827586207);

            vegaViewData.Add("Actual", 70.0, 552.903448275862);

            vegaViewData.Add("Actual", 71.0, 502.15862068965515);

            vegaViewData.Add("Actual", 72.0, 508.87827586206896);

            vegaViewData.Add("Actual", 73.0, 462.50275862068963);

            vegaViewData.Add("Actual", 74.0, 455.1955172413793);

            vegaViewData.Add("Actual", 75.0, 416.2444827586207);

            vegaViewData.Add("Actual", 76.0, 443.56172413793104);

            vegaViewData.Add("Actual", 77.0, 457.7024137931034);

            vegaViewData.Add("Actual", 78.0, 447.36413793103446);

            vegaViewData.Add("Actual", 79.0, 433.50689655172414);

            vegaViewData.Add("Actual", 80.0, 452.21172413793107);

            vegaViewData.Add("Actual", 81.0, 441.0144827586207);

            vegaViewData.Add("Actual", 82.0, 438.1520689655173);

            vegaViewData.Add("Actual", 83.0, 432.46724137931034);

            vegaViewData.Add("Actual", 84.0, 621.1710344827586);

            vegaViewData.Add("Actual", 85.0, 658.9341379310346);

            vegaViewData.Add("Actual", 86.0, 607.4920689655172);

            vegaViewData.Add("Actual", 87.0, 602.9531034482759);

            vegaViewData.Add("Actual", 88.0, 562.7348275862068);

            vegaViewData.Add("Actual", 89.0, 571.6103448275861);

            vegaViewData.Add("Actual", 90.0, 521.5062068965517);

            vegaViewData.Add("Actual", 91.0, 489.8824137931034);

            vegaViewData.Add("Actual", 92.0, 497.07482758620694);

            vegaViewData.Add("Actual", 93.0, 481.1875862068966);

            vegaViewData.Add("Actual", 94.0, 491.22275862068966);

            vegaViewData.Add("Actual", 95.0, 458.9603448275862);

            vegaViewData.Add("Actual", 96.0, 476.9755172413793);

            vegaViewData.Add("Actual", 97.0, 461.89620689655175);

            vegaViewData.Add("Actual", 98.0, 446.93379310344824);

            vegaViewData.Add("Actual", 99.0, 451.11655172413793);

            vegaViewData.Add("Actual", 100.0, 444.2193103448276);

            vegaViewData.Add("Actual", 101.0, 469.00931034482755);

            vegaViewData.Add("Actual", 102.0, 435.2944827586207);

            vegaViewData.Add("Actual", 103.0, 451.0420689655173);

            vegaViewData.Add("Actual", 104.0, 436.62793103448274);

            vegaViewData.Add("Actual", 105.0, 419.5458620689655);

            vegaViewData.Add("Actual", 106.0, 434.31482758620695);

            vegaViewData.Add("Actual", 107.0, 413.47);

            vegaViewData.Add("Actual", 108.0, 416.48517241379307);

            vegaViewData.Add("Actual", 109.0, 417.7606896551724);

            vegaViewData.Add("Actual", 110.0, 400.49586206896555);

            vegaViewData.Add("Actual", 111.0, 407.2134482758621);

            vegaViewData.Add("Actual", 112.0, 407.2334482758621);

            vegaViewData.Add("Actual", 113.0, 431.0686206896552);

            vegaViewData.Add("Actual", 114.0, 396.6813793103448);

            vegaViewData.Add("Actual", 115.0, 404.1189655172414);

            vegaViewData.Add("Actual", 116.0, 422.84896551724137);

            vegaViewData.Add("Actual", 117.0, 395.75931034482755);

            vegaViewData.Add("Actual", 118.0, 398.11931034482757);

            vegaViewData.Add("Actual", 119.0, 380.37448275862073);

            vegaViewData.Add("Actual", 120.0, 380.57965517241377);

            vegaViewData.Add("Actual", 121.0, 382.74724137931037);

            vegaViewData.Add("Actual", 122.0, 382.87);

            vegaViewData.Add("Actual", 123.0, 378.1875862068966);

            vegaViewData.Add("Actual", 124.0, 376.3451724137931);

            vegaViewData.Add("Actual", 125.0, 383.9851724137931);

            vegaViewData.Add("Actual", 126.0, 423.42137931034483);

            vegaViewData.Add("Actual", 127.0, 415.1786206896552);

            vegaViewData.Add("Actual", 128.0, 399.13413793103445);

            vegaViewData.Add("Actual", 129.0, 438.10068965517246);

            vegaViewData.Add("Actual", 130.0, 448.44586206896554);

            vegaViewData.Add("Actual", 131.0, 409.1089655172414);

            vegaViewData.Add("Simulated", 1.0, 3385.653031759629);

            vegaViewData.Add("Simulated", 2.0, 2860.142695744137);

            vegaViewData.Add("Simulated", 3.0, 2589.8935768088877);

            vegaViewData.Add("Simulated", 4.0, 2410.1319948087544);

            vegaViewData.Add("Simulated", 5.0, 2272.281066193681);

            vegaViewData.Add("Simulated", 6.0, 2158.9606020927845);

            vegaViewData.Add("Simulated", 7.0, 2058.099696923838);

            vegaViewData.Add("Simulated", 8.0, 1967.3263832210368);

            vegaViewData.Add("Simulated", 9.0, 1888.7559318476053);

            vegaViewData.Add("Simulated", 10.0, 1812.8542098290668);

            vegaViewData.Add("Simulated", 11.0, 1741.0646877304623);

            vegaViewData.Add("Simulated", 12.0, 1675.8600631031659);

            vegaViewData.Add("Simulated", 13.0, 1616.7913313085971);

            vegaViewData.Add("Simulated", 14.0, 1562.9005952937473);

            vegaViewData.Add("Simulated", 15.0, 1512.9861375432945);

            vegaViewData.Add("Simulated", 16.0, 1465.5909241922395);

            vegaViewData.Add("Simulated", 17.0, 1420.2454569752342);

            vegaViewData.Add("Simulated", 18.0, 1376.9897544488738);

            vegaViewData.Add("Simulated", 19.0, 1335.6875044498745);

            vegaViewData.Add("Simulated", 20.0, 1296.4569820854904);

            vegaViewData.Add("Simulated", 21.0, 1259.7660978603387);

            vegaViewData.Add("Simulated", 22.0, 1225.8312256311858);

            vegaViewData.Add("Simulated", 23.0, 1194.5824949972155);

            vegaViewData.Add("Simulated", 24.0, 1165.858145883228);

            vegaViewData.Add("Simulated", 25.0, 1139.4497227645202);

            vegaViewData.Add("Simulated", 26.0, 1115.0873127871596);

            vegaViewData.Add("Simulated", 27.0, 1092.451876576308);

            vegaViewData.Add("Simulated", 28.0, 1071.176627515245);

            vegaViewData.Add("Simulated", 29.0, 1050.871038256046);

            vegaViewData.Add("Simulated", 30.0, 1031.168864720826);

            vegaViewData.Add("Simulated", 31.0, 1011.8110288266415);

            vegaViewData.Add("Simulated", 32.0, 992.749498237783);

            vegaViewData.Add("Simulated", 33.0, 974.0936280546399);

            vegaViewData.Add("Simulated", 34.0, 955.9626756453326);

            vegaViewData.Add("Simulated", 35.0, 938.4214274539693);

            vegaViewData.Add("Simulated", 36.0, 921.4897698263893);

            vegaViewData.Add("Simulated", 37.0, 905.1652347629425);

            vegaViewData.Add("Simulated", 38.0, 889.434743692923);

            vegaViewData.Add("Simulated", 39.0, 874.2823897037176);

            vegaViewData.Add("Simulated", 40.0, 859.692247656633);

            vegaViewData.Add("Simulated", 41.0, 845.6459278296646);

            vegaViewData.Add("Simulated", 42.0, 832.1250771900033);

            vegaViewData.Add("Simulated", 43.0, 819.1105451802897);

            vegaViewData.Add("Simulated", 44.0, 806.5810226860616);

            vegaViewData.Add("Simulated", 45.0, 794.514475527713);

            vegaViewData.Add("Simulated", 46.0, 782.8889550036005);

            vegaViewData.Add("Simulated", 47.0, 771.6817018708116);

            vegaViewData.Add("Simulated", 48.0, 760.8693134596393);

            vegaViewData.Add("Simulated", 49.0, 750.4284750935386);

            vegaViewData.Add("Simulated", 50.0, 740.3382319647167);

            vegaViewData.Add("Simulated", 51.0, 730.5837281701004);

            vegaViewData.Add("Simulated", 52.0, 721.1552109326782);

            vegaViewData.Add("Simulated", 53.0, 712.0414957052374);

            vegaViewData.Add("Simulated", 54.0, 703.2274289878537);

            vegaViewData.Add("Simulated", 55.0, 694.6951597818771);

            vegaViewData.Add("Simulated", 56.0, 686.424985369958);

            vegaViewData.Add("Simulated", 57.0, 678.3947494874112);

            vegaViewData.Add("Simulated", 58.0, 670.579468663986);

            vegaViewData.Add("Simulated", 59.0, 662.9535447293721);

            vegaViewData.Add("Simulated", 60.0, 655.4943215059045);

            vegaViewData.Add("Simulated", 61.0, 648.1868637792459);

            vegaViewData.Add("Simulated", 62.0, 641.0282347910644);

            vegaViewData.Add("Simulated", 63.0, 634.025784190688);

            vegaViewData.Add("Simulated", 64.0, 627.1903296684648);

            vegaViewData.Add("Simulated", 65.0, 620.5308354373752);

            vegaViewData.Add("Simulated", 66.0, 614.0527978087471);

            vegaViewData.Add("Simulated", 67.0, 607.7580336491902);

            vegaViewData.Add("Simulated", 68.0, 601.6446298322244);

            vegaViewData.Add("Simulated", 69.0, 595.7073754059601);

            vegaViewData.Add("Simulated", 70.0, 589.9387326022625);

            vegaViewData.Add("Simulated", 71.0, 584.3299296285528);

            vegaViewData.Add("Simulated", 72.0, 578.8714225079025);

            vegaViewData.Add("Simulated", 73.0, 573.5522707101682);

            vegaViewData.Add("Simulated", 74.0, 568.359113477507);

            vegaViewData.Add("Simulated", 75.0, 563.275977800231);

            vegaViewData.Add("Simulated", 76.0, 558.2855119838997);

            vegaViewData.Add("Simulated", 77.0, 553.3711313135605);

            vegaViewData.Add("Simulated", 78.0, 548.5187627466465);

            vegaViewData.Add("Simulated", 79.0, 543.7172211616356);

            vegaViewData.Add("Simulated", 80.0, 538.9573867326462);

            vegaViewData.Add("Simulated", 81.0, 534.2311133394368);

            vegaViewData.Add("Simulated", 82.0, 529.530677550363);

            vegaViewData.Add("Simulated", 83.0, 524.848934531839);

            vegaViewData.Add("Simulated", 84.0, 520.1799526657965);

            vegaViewData.Add("Simulated", 85.0, 515.5197721112074);

            vegaViewData.Add("Simulated", 86.0, 510.8668373359399);

            vegaViewData.Add("Simulated", 87.0, 506.2218817474353);

            vegaViewData.Add("Simulated", 88.0, 501.5873892238552);

            vegaViewData.Add("Simulated", 89.0, 496.9668528861871);

            vegaViewData.Add("Simulated", 90.0, 492.3640330015441);

            vegaViewData.Add("Simulated", 91.0, 487.78240484972196);

            vegaViewData.Add("Simulated", 92.0, 483.22487901014574);

            vegaViewData.Add("Simulated", 93.0, 478.6937700299449);

            vegaViewData.Add("Simulated", 94.0, 474.1909118984074);

            vegaViewData.Add("Simulated", 95.0, 469.7177880082405);

            vegaViewData.Add("Simulated", 96.0, 465.27563401601617);

            vegaViewData.Add("Simulated", 97.0, 460.86554961992226);

            vegaViewData.Add("Simulated", 98.0, 456.48862821340566);

            vegaViewData.Add("Simulated", 99.0, 452.1460364158983);

            vegaViewData.Add("Simulated", 100.0, 447.8389442429914);

            vegaViewData.Add("Simulated", 101.0, 443.56831413587776);

            vegaViewData.Add("Simulated", 102.0, 439.3347030942092);

            vegaViewData.Add("Simulated", 103.0, 435.1382195712372);

            vegaViewData.Add("Simulated", 104.0, 430.97864050351023);

            vegaViewData.Add("Simulated", 105.0, 426.8556030083277);

            vegaViewData.Add("Simulated", 106.0, 422.7687866738485);

            vegaViewData.Add("Simulated", 107.0, 418.7180438099082);

            vegaViewData.Add("Simulated", 108.0, 414.70347444550964);

            vegaViewData.Add("Simulated", 109.0, 410.7254541948499);

            vegaViewData.Add("Simulated", 110.0, 406.7846054797985);

            vegaViewData.Add("Simulated", 111.0, 402.88170606104785);

            vegaViewData.Add("Simulated", 112.0, 399.01757967829565);

            vegaViewData.Add("Simulated", 113.0, 395.1930402520451);

            vegaViewData.Add("Simulated", 114.0, 391.40890502378994);

            vegaViewData.Add("Simulated", 115.0, 387.66601379634034);

            vegaViewData.Add("Simulated", 116.0, 383.9651760068833);

            vegaViewData.Add("Simulated", 117.0, 380.3070253962728);

            vegaViewData.Add("Simulated", 118.0, 376.69184439895514);

            vegaViewData.Add("Simulated", 119.0, 373.11945249512513);

            vegaViewData.Add("Simulated", 120.0, 369.5892065384409);

            vegaViewData.Add("Simulated", 121.0, 366.1000908610182);

            vegaViewData.Add("Simulated", 122.0, 362.6508441513587);

            vegaViewData.Add("Simulated", 123.0, 359.24008340236287);

            vegaViewData.Add("Simulated", 124.0, 355.86640710268074);

            vegaViewData.Add("Simulated", 125.0, 352.5284655352057);

            vegaViewData.Add("Simulated", 126.0, 349.2249830279593);

            vegaViewData.Add("Simulated", 127.0, 345.95472643069775);

            vegaViewData.Add("Simulated", 128.0, 342.71644161092155);

            vegaViewData.Add("Simulated", 129.0, 339.5088033133075);

            vegaViewData.Add("Simulated", 130.0, 336.33041659109745);

            vegaViewData.Add("Simulated", 131.0, 333.1798739853478);

            #endregion

            vegaViewData.BuildCumulativeProduction(8);

            foreach(var data in vegaViewData["Actual"])
            {
                Console.WriteLine(data);
            }

            vegaViewData.ToMonthlyProduction(new System.DateTime(2012, 10, 15), 15);

            vegaViewData.BuildCumulativeProduction(8);

            foreach(var data in vegaViewData["Actual"])
            {
                Console.WriteLine(data);
            }
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

//string api = "42-285-33615".Replace("-", "");

// using LiteDatabase database = new LiteDatabase("C:/Users/tehgo/RRC.Texas.db");
// LiteCollection<SpecificLeaseProductionQueryData> collection = database.GetCollection<SpecificLeaseProductionQueryData>(nameof(SpecificLeaseProductionQueryData));
//
// SpecificLeaseProductionQueryData[] records = collection.Query().Where(Query.EQ("API",
//                                                                                api)).ToArray();
