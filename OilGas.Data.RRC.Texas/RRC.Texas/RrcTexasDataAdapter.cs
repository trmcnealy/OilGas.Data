using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading.Tasks;

using AngleSharp.Html.Dom;

using Engineering.DataSource;
using Engineering.DataSource.OilGas;

using Microsoft.EntityFrameworkCore;

namespace OilGas.Data.RRC.Texas
{
    public sealed class RrcTexasDataAdapter : IDisposable
    {
        private readonly OilGasDbContext _context;

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public RrcTexasDataAdapter()
        {
            _context = new OilGasDbContext();
        }

        public void Dispose()
        {
            _context.SaveChanges();
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public async Task<IEnumerable<LeaseReport>> GetLeaseByApi(ApiNumber api)
        {
            IHtmlDocument htmlDoc = await QueryBuilder.WellboreQueryByApi(api);

            List<WellboreQueryData> wellboreQueriesData = QueryParser.ParseWellboreQuery(htmlDoc);

            List<LeaseReport> leases = new List<LeaseReport>(10);

            foreach(WellboreQueryData wellboreQueryData in wellboreQueriesData)
            {
                LeaseDetailQueryData leaseDetailQueryData = await QueryBuilder.LeaseDetailQuery(wellboreQueryData);

                leases.Add(LeaseReport.Create(wellboreQueryData, leaseDetailQueryData));
            }

            return leases;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public async Task<Well> GetMonthlyProductionByApi(ApiNumber api,
                                                          bool      persistentData = true,
                                                          bool      updateData     = true)
        {
            Well well;

            if(persistentData && !updateData)
            {
                try
                {
                    well = await _context.GetWellByApiAsync(api);

                    if(well?.MonthlyProduction != null)
                    {
                        return well;
                    }
                }
                catch(Exception ex)
                {
                    Debug.WriteLine(ex.Message);
                }
            }

            if(!updateData)
            {
                return null;
            }

            (string csvData, LeaseReport lease) = await QueryBuilder.SpecificLeaseProductionQueryByApi(api);

            CsvReader csvReader = new CsvReader(csvData);

            (List<string[]> header, List<string[]> rows) data = csvReader.ReadFile(10);

            List<SpecificLeaseProductionQueryData> productionData = new List<SpecificLeaseProductionQueryData>(data.rows.Count);

            foreach(string[] entry in data.rows)
            {
                productionData.Add(new SpecificLeaseProductionQueryData(api, entry));
            }

            well = ConvertFrom(lease, productionData);

            if(persistentData)
            {
                Well record = await _context.GetWellByApiAsync(api);

                if(record != null)
                {
                    record.MonthlyProduction = well.MonthlyProduction;
                    _context.Update(record);
                }
                else
                {
                    record = well;
                    await _context.AddAsync(record);
                }

                await _context.SaveChangesAsync();

                return record;
            }

            return well;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        internal Well ConvertFrom(LeaseReport                                   leaseReport,
                                  IEnumerable<SpecificLeaseProductionQueryData> queryData)
        {
            List<SpecificLeaseProductionQueryData> dataRows = queryData.ToList();

            if(dataRows.Count == 0)
            {
                throw new Exception();
            }

            SpecificLeaseProductionQueryData firstRow = dataRows.FirstOrDefault();

            if(firstRow == null)
            {
                throw new Exception();
            }

            SpecificLeaseProductionQueryData lastRow = dataRows.LastOrDefault();

            if(lastRow == null)
            {
                throw new Exception();
            }

            CumulativeProduction lastMonth = null;
            int                  month     = 0;

            List<MonthlyProduction>    monthlyProduction    = new List<MonthlyProduction>();
            List<CumulativeProduction> cumulativeProduction = new List<CumulativeProduction>();

            foreach(SpecificLeaseProductionQueryData dataRow in dataRows)
            {
                if(!float.TryParse(dataRow.OIL_BBL_Production, out float monthlyOil))
                {
                    monthlyOil = 0.0f;
                }

                if(!float.TryParse(dataRow.Casinghead_MCF_Production, out float monthlyGas))
                {
                    monthlyGas = 0.0f;
                }

                monthlyProduction.Add(new MonthlyProduction(dataRow.Api, new ProductionDate(dataRow.Date), monthlyGas, monthlyOil, 0.0));

                if(lastMonth == null)
                {
                    lastMonth = new CumulativeProduction(dataRow.Api, new ProductionDate(dataRow.Date), monthlyGas, monthlyOil, 0.0);
                    cumulativeProduction.Add(lastMonth);
                }
                else
                {
                    lastMonth = new CumulativeProduction(dataRow.Api, new ProductionDate(dataRow.Date), monthlyGas + lastMonth.GasVolume, monthlyOil + lastMonth.OilVolume, 0.0);
                    cumulativeProduction.Add(lastMonth);
                }

                ++month;
            }

            Company company = GetOperator(int.Parse(firstRow.Operator_No)) ?? new Company(firstRow.Operator_Name, firstRow.Operator_No);

            _context.AddorUpdate(company);

            Field field = GetField(int.Parse(firstRow.Field_No)) ?? new Field(firstRow.Field_Name, firstRow.Field_No);

            _context.AddorUpdate(field);

            _context.AddRange(monthlyProduction);

            _context.AddRange(cumulativeProduction);

            Well well = new Well(firstRow.Api)
            {
                Number               = leaseReport.Number,
                LeaseNumber          = leaseReport.LeaseNumber,
                Company              = company,
                Field                = field,
                Name                 = leaseReport.Name,
                MonthlyProduction    = monthlyProduction,
                CumulativeProduction = cumulativeProduction
            };

            return well;
        }

        // [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        // public async Task<Well> GetDirectionalSurvey(Well well)
        // {
        //     List<DirectionalSurveyReport> directionalSurveyReport = await QueryBuilder.CompletionReportQueryByApi(well.Api);
        //
        //     double min = directionalSurveyReport.Min(ds => ds.From);
        //     double max = directionalSurveyReport.Max(ds => ds.To);
        //
        //     well.DirectionalSurvey = new DirectionalSurvey(min, max);
        //
        //     return well;
        // }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public async Task UpdateLocationAsync(Dictionary<ApiNumber, WellS> wellS)
        {
            List<Well> wells = _context.Wells.Include(wp => wp.Location).ToList();

            foreach(Well well in wells)
            {
                if(wellS.TryGetValue(well.Api, out WellS match))
                {
                    if(well.Location == null)
                    {
                        well.Location = new Location(well.Api, match.LAT83, match.LONG83);
                    }
                    else
                    {
                        well.Location.Latitude  = match.LAT83;
                        well.Location.Longitude = match.LONG83;
                    }

                    Update(well);
                }
            }

            await Task.FromResult(Task.CompletedTask);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public async Task<Well> GetLocationAsync(Well well)
        {
            return await GetLocationAsync(well.Api);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public async Task<Well> GetLocationAsync(ApiNumber api)
        {
            GisQuery gisQuery = await QueryBuilder.GisQueryByApi(api);

            GisQuery.GisFeature feature = gisQuery?.Features.FirstOrDefault();

            Well well = await _context.Wells.Include(wp => wp.Location).FirstOrDefaultAsync(p => p.Api == api);

            //Location location = await _context.Locations.FirstOrDefaultAsync(p => p.Api == api);

            if(feature != null)
            {
                if(well.Location == null)
                {
                    well.Location = new Location(well.Api, feature.Attributes.GisLat83, feature.Attributes.GisLong83);
                    //await _context.AddAsync(well.Location);
                }
                else
                {
                    well.Location.Latitude  = feature.Attributes.GisLat83;
                    well.Location.Longitude = feature.Attributes.GisLong83;
                    //_context.Update(well.Location);
                }

                Update(well);
            }

            return well;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public async Task<string> GetG1ReportAsync(Well well)
        {
            return await GetG1ReportAsync(well.Api);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public async Task<string> GetG1ReportAsync(ApiNumber api)
        {
            return await QueryBuilder.CompletionReportG1QueryByApi(api);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public async Task<Well> GetReportsAsync(Well well)
        {
            string report = await QueryBuilder.CompletionReportsQueryByApi(well.Api);

            return well;
        }

        //public async Task<Location> GetLocationByApi(ApiNumber api)
        //{
        //    //try
        //    //{
        //    //    Registry record = await FracFocusRegistry.FirstAsync(w => w.ApiNumber == api);

        //    //    if(record != null)
        //    //    {
        //    //        return new WellLocation(record.Latitude, record.Longitude, record.Projection);
        //    //    }
        //    //}
        //    //catch(Exception ex)
        //    //{
        //    //    Debug.WriteLine(ex.Message);
        //    //}

        //    return null;
        //}

        //[MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        //public void Optimize()
        //{
        //    _context.Optimize();
        //}

        //[MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        //public void Compact()
        //{
        //    _context.Compact();
        //}

        //[MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        //public void Backup()
        //{
        //    _context.Backup();
        //}

        //[MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        //public void LoadDb(string filePath)
        //{
        //    _context.LoadDb(filePath);
        //}

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public void LoadOperatorsCsv(string filePath)
        {
            _context.LoadOperatorsCsv(filePath);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public void LoadDrillingPermitsCsv(string filePath)
        {
            _context.LoadDrillingPermitsCsv(filePath);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public void LoadFracFocusRegistryCsv(string filePath)
        {
            _context.LoadFracFocusRegistryCsv(filePath);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public List<Well> GetAllWellsIncludingAsync()
        {
            return _context.GetAllWellsIncluding();
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public List<DrillingPermit> GetDrillingPermits()
        {
            return _context.DrillingPermits.ToList();
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public void Add(Well well)
        {
            _context.Add(well);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public async Task AddAsync(Well well)
        {
            await _context.AddAsync(well);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public void AddRange(List<Well> wells)
        {
            _context.AddRange(wells);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public async Task AddRangeAsync(List<Well> wells)
        {
            await _context.AddRangeAsync(wells);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public void Update(Well well)
        {
            _context.Update(well);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public void Commit()
        {
            _context.Commit();
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public async Task CommitAsync()
        {
            await _context.CommitAsync();
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public List<Well> GetWellsByOperator(string name)
        {
            return _context.GetWellsByOperator(name);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public async Task<List<Well>> GetWellsByOperatorAsync(string name)
        {
            return await _context.GetWellsByOperatorAsync(name);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public Well GetWellByApi(ApiNumber api)
        {
            return _context.GetWellByApi(api);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public Field GetField(int number)
        {
            return _context.GetField(number);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public async Task<Field> GetFieldAsync(int number)
        {
            return await _context.GetFieldAsync(number);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public Field GetField(string name)
        {
            return _context.GetField(name);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public async Task<Field> GetFieldAsync(string name)
        {
            return await _context.GetFieldAsync(name);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public Company GetOperator(int number)
        {
            return _context.GetOperator(number);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public async Task<Company> GetOperatorAsync(int number)
        {
            return await _context.GetOperatorAsync(number);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public Company GetOperator(string name)
        {
            return _context.GetOperator(name);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public async Task<Company> GetOperatorAsync(string name)
        {
            return await _context.GetOperatorAsync(name);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public void GetDrillingPermitsByCounty(CountyType countyType,
                                               DateTime   approvedDateFrom,
                                               DateTime   approvedDateTo)
        {
            // CountyType.Kind[] counties = new[]
            // {
            //     CountyType.Kind.ATASCOSA, CountyType.Kind.DE_WITT, CountyType.Kind.LA_SALLE, CountyType.Kind.DUVAL, CountyType.Kind.LIVE_OAK, CountyType.Kind.KARNES, CountyType.Kind.FRIO,
            //     CountyType.Kind.MCMULLEN, CountyType.Kind.WEBB, CountyType.Kind.ZAVALA, CountyType.Kind.MAVERICK
            // };

            string data = QueryBuilder.DrillingPermitsQueryByCounty(countyType, approvedDateFrom, approvedDateTo).Result;

            string tempFile = Path.GetTempFileName();

            File.WriteAllText(tempFile, data);

            LoadDrillingPermitsCsv(tempFile);

            if(File.Exists(tempFile))
            {
                File.Delete(tempFile);
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public async Task UpdateAllWellsMonthlyProductionAsync()
        {
            List<ApiNumber> wellApis = _context.Wells.Select(p => p.Api).ToList();

            foreach(ApiNumber api in wellApis)
            {
                try
                {
                    await GetMonthlyProductionByApi(api);
                }
                catch(Exception)
                {
                    //
                }
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public async Task UpdateAllWellsLocationsAsync()
        {
            List<ApiNumber> wellApis = _context.Wells.Where(w => w.Location == null || w.Location.Latitude == null || w.Location.Longitude == null).Select(p => p.Api).ToList();

            foreach(ApiNumber api in wellApis)
            {
                try
                {
                    await GetLocationAsync(api);
                }
                catch(Exception)
                {
                    await Console.Error.WriteLineAsync($"GetLocationAsync failed for {api}");
                }
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public async Task UpdateAllWellsHydrocarbonPropertiesAsync()
        {
            List<string> downloadedG1Files = Directory.GetFiles(@"D:\OilGasData", "*.G1.pdf").Select(f => Path.GetFileName(f).Substring(0, 18)).ToList();

            //Parallel.ForEach(Partitioner.Create(0, downloadedG1Files.Count, downloadedG1Files.Count / Environment.ProcessorCount),
            //                 row =>
            //                 {
            ApiNumber api;
            string    filePath;

            double? density               = null;
            double? visocity              = null;
            double? formationVolumeFactor = null;
            double? compressibility       = null;
            double? referenceTemperature  = null;
            double? referencePressure     = null;

            double? h2S = null;
            double? co2 = null;
            double? n2  = null;

            //for(int i = row.Item1; i < row.Item2; i++)
            for(int i = 0; i < downloadedG1Files.Count; i++)
            {
                try
                {
                    api = downloadedG1Files[i];

                    filePath = Path.Combine(@"D:\OilGasData", api + ".G1.pdf");

                    if(api == "42-255-31650-00-00")
                    {
                        Debugger.Break();
                    }

                    (DepthScanner depth, OilGasPropertiesScanner oilGasProperties) = CompletionReportReader.ReadReport(filePath);

                    (double tvdDepth, double totalDepth) = (depth.TvdDepth, depth.TotalDepth);

                    if(Math.Abs(oilGasProperties.OilApiGravity) < double.Epsilon && Math.Abs(oilGasProperties.GasSpecificGravity) < double.Epsilon)
                    {
                        continue;
                    }

                    (double gasSpecificGravity, double oilApiGravity, double gasLiquidHydroRatio, double avgShutinTemperature, double bottomHoleTemperature, double bottomHoleDepth) =
                        (oilGasProperties.GasSpecificGravity, oilGasProperties.OilApiGravity, oilGasProperties.GasLiquidHydroRatio, oilGasProperties.AvgShutinTemperature,
                         oilGasProperties.BottomHoleTemperature, oilGasProperties.BottomHoleDepth);

                    //TODO Pressure
                    double pressure = tvdDepth * 0.65;

                    double Rs = Pvt.GasSolubility.VasquezBeggs(4815.0, 74.0, oilApiGravity, gasSpecificGravity);

                    density               = oilApiGravity;
                    visocity              = Pvt.OilViscosity.Saturated.PetroskyFarshad(Pvt.OilViscosity.Dead.Beal(bottomHoleTemperature, oilApiGravity), gasLiquidHydroRatio);
                    formationVolumeFactor = Pvt.OilFormationVolumeFactor.VasquezBeggs(gasLiquidHydroRatio, bottomHoleTemperature, oilApiGravity, gasSpecificGravity);
                    compressibility       = Pvt.OilCompressibility.VasquezBeggs(gasLiquidHydroRatio, pressure, bottomHoleTemperature, oilApiGravity, gasSpecificGravity);
                    referenceTemperature  = bottomHoleTemperature;
                    referencePressure     = null;

                    OilProperties oilProperties = new OilProperties(api, density, visocity, formationVolumeFactor, compressibility, referenceTemperature, referencePressure);

                    await _context.AddorUpdateAsync(oilProperties);

                    density              = gasSpecificGravity;
                    h2S                  = null;
                    co2                  = null;
                    n2                   = null;
                    visocity             = Pvt.GasViscosity.CarrKobayashiBurrows(pressure, bottomHoleTemperature, gasSpecificGravity);
                    compressibility      = Pvt.GasCompressibility.ZFactor(pressure, bottomHoleTemperature, gasSpecificGravity);
                    referenceTemperature = bottomHoleTemperature;
                    referencePressure    = null;

                    GasProperties gasProperties = new GasProperties(api, gasSpecificGravity, h2S, co2, n2, visocity, compressibility, referenceTemperature, referencePressure);

                    await _context.AddorUpdateAsync(gasProperties);

                    CompletionDetails completionDetails = new CompletionDetails(api, null, null, totalDepth - tvdDepth);

                    await _context.AddorUpdateAsync(completionDetails);

                    Well well = _context.Wells.Include(wp => wp.CompletionDetails)
                                        .Include(wp => wp.GasProperties)
                                        .Include(wp => wp.OilProperties)
                                        .Include(wp => wp.WaterProperties)
                                        .FirstOrDefault(p => p.Api == api);

                    well.OilProperties     = oilProperties;
                    well.GasProperties     = gasProperties;
                    well.CompletionDetails = completionDetails;

                    _context.Update(well);
                }
                catch(Exception)
                {
                    //
                }
            }
            //});

            await Task.FromResult(Task.CompletedTask);
        }
    }
}
