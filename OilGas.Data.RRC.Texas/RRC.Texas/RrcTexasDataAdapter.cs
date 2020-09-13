#nullable enable
using AngleSharp.Html.Dom;

using Engineering.DataSource;
using Engineering.DataSource.CoordinateSystems;
using Engineering.DataSource.OilGas;

using Microsoft.EntityFrameworkCore;

using Npgsql;

using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Data;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading.Tasks;

namespace OilGas.Data.RRC.Texas
{
    public sealed class RrcTexasDataAdapter : IDisposable
    {
        private readonly OilGasDbContext _context;

        public OilGasDbContext Context { get { return _context; } }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public RrcTexasDataAdapter()
        {
            _context = new OilGasDbContext();
        }

        public void CloseConnection()
        {
            _context.Connection.Close();
        }

        public void Dispose()
        {
            _context.SaveChanges();
        }

        public void ReIndexAllTables()
        {
            NpgsqlCommand command = Context.Connection.CreateCommand();

            ReIndex(command, "ShapeFileLocation");

            ReIndex(command, "Company");

            ReIndex(command, "DrillingPermit");

            ReIndex(command, "DirectionalSurvey");

            ReIndex(command, "Field");

            ReIndex(command, "Lease");

            ReIndex(command, "Location");

            ReIndex(command, "WellboreDetails");

            ReIndex(command, "CompletionDetails");

            ReIndex(command, "ReservoirData");

            ReIndex(command, "PerforationInterval");

            ReIndex(command, "OilProperties");

            ReIndex(command, "GasProperties");

            ReIndex(command, "WaterProperties");

            ReIndex(command, "ReservoirProperties");

            ReIndex(command, "DailyProduction");

            ReIndex(command, "MonthlyProduction");

            ReIndex(command, "CumulativeProduction");

            ReIndex(command, "Well");
        }

        public void VacuumDb()
        {
            NpgsqlCommand command = Context.Connection.CreateCommand();

            command.CommandText = $"VACUUM (FULL) \"OilGas\";";

            command.ExecuteNonQuery();
        }

        public void ReIndexDb()
        {
            NpgsqlCommand command = Context.Connection.CreateCommand();

            command.CommandText = $"REINDEX DATABASE CONCURRENTLY \"OilGas\";";

            command.ExecuteNonQuery();
        }

        public void ReIndex(NpgsqlCommand command,
                            string        tableName)
        {
            command.CommandText = $"REINDEX TABLE CONCURRENTLY \"{tableName}\";";

            command.ExecuteNonQuery();
        }

        public void ReIndex(string tableName)
        {
            NpgsqlCommand command = Context.Connection.CreateCommand();

            command.CommandText = $"REINDEX TABLE CONCURRENTLY \"{tableName}\";";

            command.ExecuteNonQuery();
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public void Add(ShapeFileLocation location)
        {
            _context.Add(location);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public void AddRange(List<ShapeFileLocation> locations)
        {
            _context.AddRange(locations);
        }

        public void ConvertLocations()
        {
            _context.ConvertLocations();
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
        public async Task<Well?> GetMonthlyProductionFromWebsiteByApi(ApiNumber api)
        {
            (string csvData, LeaseReport lease) = await QueryBuilder.SpecificLeaseProductionQueryByApi(api);

            CsvReader csvReader = new CsvReader(csvData);

            (List<string[]> header, List<string[]> rows) data = csvReader.ReadFile(10);

            List<SpecificLeaseProductionQueryData> productionData = new List<SpecificLeaseProductionQueryData>(data.rows.Count);

            foreach(string[] entry in data.rows)
            {
                productionData.Add(new SpecificLeaseProductionQueryData(api, entry));
            }

            Well? well = ConvertFrom(lease, productionData, lease.DistrictCode);

            return well;
        }

        //[MethodImpl(MethodImplOptions.AggressiveInlining |
        //MethodImplOptions.AggressiveOptimization)] public async Task<Well?>
        // GetMonthlyProductionByApi(ApiNumber api,
        //                                                   bool      persistentData =
        //                                                   true, bool      updateData
        //                                                   = true)
        //{
        //    Well well;

        //    if(persistentData && !updateData)
        //    {
        //        try
        //        {
        //            List<MonthlyProduction> monthlyProduction =
        //            _context.MonthlyProduction.Where(w => w.Api == api).ToList();

        //            well = await _context.GetWellByApiAsync(api);

        //            if(well?.MonthlyProduction.Count == 0 && monthlyProduction.Count >
        //            0)
        //            {
        //                well.MonthlyProduction = monthlyProduction;

        //                List<CumulativeProduction> cumulativeProduction =
        //                _context.CumulativeProduction.Where(w => w.Api ==
        //                api).ToList();

        //                well.CumulativeProduction = cumulativeProduction;

        //                _context.Update(well);
        //            }

        //            if(well?.MonthlyProduction.Count > 0)
        //            {
        //                return well;
        //            }
        //        }
        //        catch(Exception ex)
        //        {
        //            Debug.WriteLine(ex.Message);
        //        }
        //    }

        //    if(!updateData)
        //    {
        //        return null;
        //    }

        //    (string csvData, LeaseReport lease) = await
        //    QueryBuilder.SpecificLeaseProductionQueryByApi(api);

        //    CsvReader csvReader = new CsvReader(csvData);

        //    (List<string[]> header, List<string[]> rows) data =
        //    csvReader.ReadFile(10);

        //    List<SpecificLeaseProductionQueryData> productionData = new
        //    List<SpecificLeaseProductionQueryData>(data.rows.Count);

        //    foreach(string[] entry in data.rows)
        //    {
        //        productionData.Add(new SpecificLeaseProductionQueryData(api, entry));
        //    }

        //    well = ConvertFrom(lease, productionData);

        //    if(persistentData)
        //    {
        //        Well record = await _context.GetWellByApiAsync(api);

        //        if(record != null)
        //        {
        //            record.Company              = well.Company;
        //            record.Field                = well.Field;
        //            record.MonthlyProduction    = well.MonthlyProduction;
        //            record.CumulativeProduction = well.CumulativeProduction;
        //            _context.Update(record);
        //        }
        //        else
        //        {
        //            record = well;
        //            await _context.AddAsync(record);
        //        }

        //        await _context.SaveChangesAsync();

        //        return record;
        //    }

        //    return well;
        //}

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        internal Well? ConvertFrom(LeaseReport                                   leaseReport,
                                   IEnumerable<SpecificLeaseProductionQueryData> queryData,
                                   int                                           district)
        {
            List<SpecificLeaseProductionQueryData> dataRows = queryData.ToList();

            if(dataRows.Count == 0)
            {
                throw new Exception();
            }

            CumulativeProduction? lastMonth = null;
            int                   month     = 0;

            List<MonthlyProduction>    monthlyProduction    = new List<MonthlyProduction>();
            List<CumulativeProduction> cumulativeProduction = new List<CumulativeProduction>();

            SpecificLeaseProductionQueryData? operatorDataRow = null;

            foreach(SpecificLeaseProductionQueryData dataRow in dataRows)
            {
                if(!string.IsNullOrEmpty(dataRow.Operator_Name))
                {
                    operatorDataRow = dataRow;
                }

                if(!float.TryParse(dataRow.OIL_BBL_Production, out float monthlyOil))
                {
                    monthlyOil = 0.0f;
                }

                if(!float.TryParse(dataRow.Casinghead_MCF_Production, out float monthlyGas))
                {
                    monthlyGas = 0.0f;
                }

                monthlyProduction.Add(new MonthlyProduction(new ProductionDate(dataRow.Date), monthlyGas, 0.0, monthlyOil, 0.0));

                if(lastMonth is null)
                {
                    lastMonth = new CumulativeProduction(new ProductionDate(dataRow.Date), monthlyGas, monthlyOil, 0.0);
                    cumulativeProduction.Add(lastMonth);
                }
                else
                {
                    lastMonth = new CumulativeProduction(new ProductionDate(dataRow.Date), monthlyGas + lastMonth.GasVolume, monthlyOil + lastMonth.OilVolume, 0.0);
                    cumulativeProduction.Add(lastMonth);
                }

                ++month;
            }

            if(operatorDataRow != null)
            {
                Company company = GetOperator(int.Parse(operatorDataRow.Operator_No)) ?? new Company(operatorDataRow.Operator_Name, operatorDataRow.Operator_No);

                //_context.AddorUpdate(company);

                Field field = GetField(long.Parse(operatorDataRow.Field_No), district) ?? new Field(operatorDataRow.Field_Name, long.Parse(operatorDataRow.Field_No), district);

                //_context.AddorUpdate(field);

                Well well = new Well(operatorDataRow.Api)
                {
                    Number               = leaseReport.Number,
                    Lease                = new Lease(leaseReport.Name, leaseReport.LeaseNumber),
                    Company              = company,
                    Field                = field,
                    Name                 = leaseReport.Name,
                    MonthlyProduction    = monthlyProduction,
                    CumulativeProduction = cumulativeProduction
                };

                return well;
            }

            return null;
        }

        // [MethodImpl(MethodImplOptions.AggressiveInlining |
        // MethodImplOptions.AggressiveOptimization)] public async Task<Well>
        // GetDirectionalSurvey(Well well)
        // {
        //     List<DirectionalSurveyReport> directionalSurveyReport = await
        //     QueryBuilder.CompletionReportQueryByApi(well.Api);
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
                if(wellS.TryGetValue(well.Api, out WellS? match))
                {
                    if(well.Location is null)
                    {
                        well.Location = new Location(match.LAT83, match.LONG83);
                    }
                    else
                    {
                        well.Location.Latitude83  = match.LAT83;
                        well.Location.Longitude83 = match.LONG83;
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
            GisQuery? gisQuery = await QueryBuilder.GisQueryByApi(api);

            GisQuery.GisFeature? feature = gisQuery?.Features.FirstOrDefault();

            Well well = await _context.Wells.Include(wp => wp.Location).FirstOrDefaultAsync(p => p.Api == api);

            // Location location = await _context.Locations.FirstOrDefaultAsync(p => p.Api
            // == api);

            if(feature != null)
            {
                if(well.Location is null)
                {
                    well.Location = new Location(feature.Attributes.GisLat83, feature.Attributes.GisLong83);
                    // await _context.AddAsync(well.Location);
                }
                else
                {
                    well.Location.Latitude83  = feature.Attributes.GisLat83;
                    well.Location.Longitude83 = feature.Attributes.GisLong83;
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

        // public async Task<Location> GetLocationByApi(ApiNumber api)
        //{
        //    //try
        //    //{
        //    //    Registry record = await FracFocusRegistry.FirstAsync(w =>
        //    w.ApiNumber == api);

        //    //    if(record != null)
        //    //    {
        //    //        return new WellLocation(record.Latitude, record.Longitude,
        //    record.Projection);
        //    //    }
        //    //}
        //    //catch(Exception ex)
        //    //{
        //    //    Debug.WriteLine(ex.Message);
        //    //}

        //    return null;
        //}

        //[MethodImpl(MethodImplOptions.AggressiveInlining |
        //MethodImplOptions.AggressiveOptimization)] public void Optimize()
        //{
        //    _context.Optimize();
        //}

        //[MethodImpl(MethodImplOptions.AggressiveInlining |
        //MethodImplOptions.AggressiveOptimization)] public void Compact()
        //{
        //    _context.Compact();
        //}

        //[MethodImpl(MethodImplOptions.AggressiveInlining |
        //MethodImplOptions.AggressiveOptimization)] public void Backup()
        //{
        //    _context.Backup();
        //}

        //[MethodImpl(MethodImplOptions.AggressiveInlining |
        //MethodImplOptions.AggressiveOptimization)] public void LoadDb(string filePath)
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
        public List<Well> GetAllWells()
        {
            return _context.GetAllWells();
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

        public void Remove(Well well)
        {
            _context.Remove(well);
        }

        public async Task RemoveAsync(Well well)
        {
            await _context.RemoveAsync(well);
        }

        public void RemoveRange(List<Well> wells)
        {
            _context.RemoveRange(wells);
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
        public List<Well> GetWellsByCounty(string name)
        {
            return _context.GetWellsByCounty(name);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public async Task<List<Well>> GetWellsByCountyAsync(string name)
        {
            return await _context.GetWellsByCountyAsync(name);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public Well GetWellByApi(ApiNumber api)
        {
            return _context.GetWellByApi(api);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public Task<Well> GetWellByApiAsync(ApiNumber api)
        {
            return _context.GetWellByApiAsync(api);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public Lease GetLease(long number,
                              int  district)
        {
            return _context.GetLease(number, district);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public async Task<Lease> GetLeaseAsync(long number,
                                               int  district)
        {
            return await _context.GetLeaseAsync(number, district);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public Field GetField(long number,
                              int  district)
        {
            return _context.GetField(number, district);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public async Task<Field> GetFieldAsync(long number,
                                               int  district)
        {
            return await _context.GetFieldAsync(number, district);
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
        public Company GetOperator(long number)
        {
            return _context.GetOperator(number);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public async Task<Company> GetOperatorAsync(long number)
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
            //     CountyType.Kind.ATASCOSA, CountyType.Kind.DE_WITT,
            //     CountyType.Kind.LA_SALLE, CountyType.Kind.DUVAL,
            //     CountyType.Kind.LIVE_OAK, CountyType.Kind.KARNES, CountyType.Kind.FRIO,
            //     CountyType.Kind.MCMULLEN, CountyType.Kind.WEBB, CountyType.Kind.ZAVALA,
            //     CountyType.Kind.MAVERICK
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

        //[MethodImpl(MethodImplOptions.AggressiveInlining |
        //MethodImplOptions.AggressiveOptimization)] public async Task
        // UpdateAllWellsMonthlyProductionAsync()
        //{
        //    List<ApiNumber> wellApis = _context.Wells.Select(p => p.Api).ToList();

        //    foreach(ApiNumber api in wellApis)
        //    {
        //        try
        //        {
        //            await GetMonthlyProductionByApi(api);
        //        }
        //        catch(Exception)
        //        {
        //            //
        //    }
        //}

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public async Task UpdateAllWellsLocationsAsync()
        {
            List<ApiNumber> wellApis = _context.Wells.Where(w => w.Location == null || w.Location.Latitude27 == null || w.Location.Longitude27 == null).Select(p => p.Api).ToList();

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
        public static void LoadTexasDbs(int districtNumber)
        {
            using TexasAggregateContext tac = new TexasAggregateContext();
            tac.ChangeTracker.AutoDetectChangesEnabled = false;

            ImmutableList<ProductionAggr>? productionAggrTable = tac.ProductionAggrTable.Where(w => w.DISTRICT_NO == districtNumber).ToImmutableList();

            loadTexasDbs(productionAggrTable, districtNumber);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private static void loadTexasDbs(ImmutableList<ProductionAggr> productionAggrTable,
                                         int                           districtNumber)
        {
            Dictionary<long, Company> companies = new Dictionary<long, Company>(20000);
            Dictionary<long, Lease>   leases    = new Dictionary<long, Lease>(productionAggrTable.Count);
            Dictionary<long, Field>   fields    = new Dictionary<long, Field>(20000);

            using RrcTexasDataAdapter _context = new RrcTexasDataAdapter();

            using TexasAggregateContext tac = new TexasAggregateContext();
            tac.ChangeTracker.AutoDetectChangesEnabled = false;

            long    OPERATOR_NO;
            string? OPERATOR_NAME;
            long    FIELD_NO;
            string? FIELD_NAME;
            long    LEASE_NO;
            string? LEASE_NAME;
            string? WELL_NO;

            Company? company;
            Field?   field;
            Lease?   lease;

            ProductionAggr productionAggr;

            ImmutableHashSet<ApiNumber> apis = _context.GetAllWells().Select(w => w.Api).ToImmutableHashSet();

            foreach(Company companyDb in _context.Context.Companys.ToImmutableList())
            {
                companies.Add(companyDb.Number, companyDb);
            }

            foreach(Field fieldDb in _context.Context.Fields.ToImmutableList())
            {
                fields.Add(fieldDb.Number, fieldDb);
            }

            foreach(Lease leaseDb in _context.Context.Leases.ToImmutableList().Where(leaseDb => leaseDb.Name != null))
            {
                leases.Add(leaseDb.Number, leaseDb);
            }

            for(int i = 0; i < productionAggrTable.Count; i++)
            {
                productionAggr = productionAggrTable[i];

                if(productionAggr != null)
                {
                    if(apis.Contains(productionAggr.Api))
                    {
                        continue;
                    }

                    OPERATOR_NO   = productionAggr.OPERATOR_NO;
                    OPERATOR_NAME = productionAggr.OPERATOR_NAME;

                    if(companies.ContainsKey(OPERATOR_NO))
                    {
                        company = _context.GetOperator(OPERATOR_NO) ?? companies[OPERATOR_NO];
                    }
                    else
                    {
                        company = new Company(OPERATOR_NAME, OPERATOR_NO);
                        companies.Add(OPERATOR_NO, company);
                    }

                    FIELD_NO   = productionAggr.FIELD_NO;
                    FIELD_NAME = productionAggr.FIELD_NAME;

                    if(fields.ContainsKey(FIELD_NO))
                    {
                        field = _context.GetField(FIELD_NO, districtNumber) ?? fields[FIELD_NO];
                    }
                    else
                    {
                        field = new Field(FIELD_NAME, FIELD_NO, districtNumber);
                        fields.Add(FIELD_NO, field);
                    }

                    LEASE_NO   = productionAggr.LEASE_NO;
                    LEASE_NAME = productionAggr.LEASE_NAME;

                    if(leases.ContainsKey(LEASE_NO))
                    {
                        lease = _context.GetLease(LEASE_NO, districtNumber) ?? leases[LEASE_NO];
                    }
                    else
                    {
                        lease = new Lease(LEASE_NAME, LEASE_NO, districtNumber);
                        leases.Add(LEASE_NO, lease);
                    }

                    WELL_NO = productionAggr.WELL_NO;

                    _context.Add(new Well(productionAggr.Api, WELL_NO, lease, field, company));
                }

                if(i % 100 == 0)
                {
                    Console.WriteLine($"{i}");
                    _context.Commit();
                }
            }

            tac.Connection.Close();

            _context.Commit();
            _context.CloseConnection();
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static void LoadTexasDbs2(ImmutableList<ApiNumber> apis,
                                         int                      districtNumber)
        {
            loadTexasDbs2(apis, districtNumber);
        }

        /// <summary>
        /// CompletionInformation
        /// </summary>
        /// <param name="apis"></param>
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private static void loadTexasDbs2(ImmutableList<ApiNumber> apis,
                                          int                      districtNumber)
        {
            Parallel.ForEach(Partitioner.Create(0, apis.Count),
                             row =>
                             {
                                 int threadId = System.Threading.Thread.CurrentThread.ManagedThreadId;

                                 using RrcTexasDataAdapter _context = new RrcTexasDataAdapter();

                                 using TexasWellboreContext twc = new TexasWellboreContext();
                                 twc.ChangeTracker.AutoDetectChangesEnabled = false;

                                 // ImmutableHashSet<long?>? apis = wells.Select(w =>
                                 // (long?)w.Api.GetUniqueWellIdentifier()).ToImmutableHashSet();

                                 // ImmutableList<WellBoreTechnicalDataRoot> wellBoreTechnicalDataRoots =
                                 // twc.ByApis(districtNumber);

                                 WellBoreTechnicalDataRoot? wellBoreTechnicalDataRoot;
                                 CompletionDetails?         completionDetails;
                                 WellboreDetails?           wellboreDetails;

                                 double?   WB_TOTAL_DEPTH;
                                 DateTime? CompletionDate;
                                 double    maxCasingDepth;

                                 // List<double?> WB_CASING_DEPTH_SET = new List<double?>();
                                 //List<int?>    wbPerfCount = new List<int?>();
                                 //List<double?> wbFromPerf  = new List<double?>();
                                 //List<double?> wbToPerf    = new List<double?>();
                                 //List<string?> WB_FORMATION_NAME  = new List<string?>();
                                 //List<double?> WB_FORMATION_DEPTH = new List<double?>();
                                 // List<double?> WB_WGS84_LATITUDE         = new List<double?>();
                                 // List<double?> WB_WGS84_LONGITUDE        = new List<double?>();
                                 // List<short?>  WB_PLANE_ZONE             = new List<short?>();
                                 // List<double?> WB_PLANE_COORDINATE_EAST  = new List<double?>();
                                 // List<double?> WB_PLANE_COORDINATE_NORTH = new List<double?>();

                                 Well well;

                                 for(int i = row.Item1; i < row.Item2; i++) // for(int i = 0; i < wells.Count; ++i)
                                 {
                                     well = _context.GetWellByApi(apis[i]);

                                     if(well.WellboreDetails != null)
                                     {
                                         continue;
                                     }

                                     maxCasingDepth = double.MinValue;

                                     wellBoreTechnicalDataRoot = twc.ByApi(well.Api);

                                     if(wellBoreTechnicalDataRoot != null)
                                     {
                                         WB_TOTAL_DEPTH = wellBoreTechnicalDataRoot.WB_TOTAL_DEPTH;

                                         if(well.WellboreDetails is null)
                                         {
                                             wellboreDetails      = new WellboreDetails(null, null, WB_TOTAL_DEPTH);
                                             well.WellboreDetails = wellboreDetails;
                                         }
                                         else
                                         {
                                             wellboreDetails = well.WellboreDetails;
                                         }

                                         if(well.CompletionDetails.Count                 == 0    &&
                                            wellBoreTechnicalDataRoot.WB_ORIG_COMPL_CENT != null &&
                                            wellBoreTechnicalDataRoot.WB_ORIG_COMPL_CENT > 0     &&
                                            wellBoreTechnicalDataRoot.WB_ORIG_COMPL_YY   != null &&
                                            wellBoreTechnicalDataRoot.WB_ORIG_COMPL_YY   > 0     &&
                                            wellBoreTechnicalDataRoot.WB_ORIG_COMPL_MM   != null &&
                                            wellBoreTechnicalDataRoot.WB_ORIG_COMPL_MM   > 0)
                                         {
                                             CompletionDate =
                                                 new DateTime((int)((wellBoreTechnicalDataRoot.WB_ORIG_COMPL_CENT.Value * 100) + wellBoreTechnicalDataRoot.WB_ORIG_COMPL_YY.Value),
                                                              Math.Min(12, (int)wellBoreTechnicalDataRoot.WB_ORIG_COMPL_MM),
                                                              ((wellBoreTechnicalDataRoot.WB_ORIG_COMPL_DD is null || wellBoreTechnicalDataRoot.WB_ORIG_COMPL_DD == 0)
                                                                   ? 1
                                                                   : Math.Min(31, (int)wellBoreTechnicalDataRoot.WB_ORIG_COMPL_DD.Value)));

                                             completionDetails = new CompletionDetails(CompletionDate);
                                             well.CompletionDetails?.Add(completionDetails);
                                         }
                                         else
                                         {
                                             completionDetails = well.CompletionDetails.FirstOrDefault();
                                         }

                                         if(wellBoreTechnicalDataRoot.WellBoreOldLocation != null && wellBoreTechnicalDataRoot.WellBoreOldLocation.WB_LEASE_NAME != null && well.Name is null)
                                         {
                                             if(well.Lease != null)
                                             {
                                                 well.Name = well.Lease.Name = wellBoreTechnicalDataRoot.WellBoreOldLocation.WB_LEASE_NAME;
                                             }
                                         }

                                         if(wellBoreTechnicalDataRoot.WellBoreCompletionInformation != null && wellBoreTechnicalDataRoot.WellBoreCompletionInformation.Count > 0)
                                         {
                                             foreach(var wellBoreCompletionInformation in wellBoreTechnicalDataRoot.WellBoreCompletionInformation)
                                             {
                                                 if(wellBoreCompletionInformation.WellBoreTechnicalDataFormsFileDate       != null &&
                                                    wellBoreCompletionInformation.WellBoreTechnicalDataFormsFileDate.Count > 0)
                                                 {
                                                     foreach(var wellBoreTechnicalDataFormsFileDate in wellBoreCompletionInformation.WellBoreTechnicalDataFormsFileDate)
                                                     {
                                                         // WB_DIR_SURVEY
                                                         if(wellBoreTechnicalDataFormsFileDate.WB_ELEVATION > 0)
                                                         {
                                                             wellboreDetails.Elevation = wellBoreTechnicalDataFormsFileDate.WB_ELEVATION;

                                                             wellboreDetails.ElevationDatum =
                                                                 WellBoreTechnicalDataFormsFileDate.ElevationCodeToString(wellBoreTechnicalDataFormsFileDate.WB_ELEVATION_CODE);
                                                         }

                                                         if(wellBoreTechnicalDataFormsFileDate.WellBoreCasing != null && wellBoreTechnicalDataFormsFileDate.WellBoreCasing.Count > 0)
                                                         {
                                                             foreach(var wellBoreCasing in wellBoreTechnicalDataFormsFileDate.WellBoreCasing)
                                                             {
                                                                 // WB_CASING_DEPTH_SET.Add(wellBoreCasing.WB_CASING_DEPTH_SET);

                                                                 if(wellBoreCasing.WB_CASING_DEPTH_SET != null &&
                                                                    wellBoreCasing.WB_CASING_DEPTH_SET > 0     &&
                                                                    maxCasingDepth                     < wellBoreCasing.WB_CASING_DEPTH_SET.Value!)
                                                                 {
                                                                     maxCasingDepth = (double)wellBoreCasing.WB_CASING_DEPTH_SET;
                                                                 }
                                                             }

                                                             if(wellboreDetails.TotalLength is null)
                                                             {
                                                                 wellboreDetails.TotalLength = maxCasingDepth;
                                                             }
                                                             else if(wellboreDetails.TotalLength < maxCasingDepth)
                                                             {
                                                                 wellboreDetails.TotalLength = maxCasingDepth;
                                                             }
                                                         }

                                                         if(completionDetails                                             != null &&
                                                            wellBoreTechnicalDataFormsFileDate.WellBorePerforations       != null &&
                                                            wellBoreTechnicalDataFormsFileDate.WellBorePerforations.Count > 0     &&
                                                            completionDetails.PerforationIntervals.Count                  == 0)
                                                         {
                                                             foreach(var wellBorePerforations in wellBoreTechnicalDataFormsFileDate.WellBorePerforations)
                                                             {
                                                                 completionDetails.PerforationIntervals.Add(new PerforationInterval(wellBorePerforations.WB_PERF_COUNT,
                                                                                                                wellBorePerforations.WB_FROM_PERF,
                                                                                                                wellBorePerforations.WB_TO_PERF));
                                                             }
                                                         }

                                                         if(wellBoreTechnicalDataFormsFileDate.WellBoreFormation != null && wellBoreTechnicalDataFormsFileDate.WellBoreFormation.Count > 0)
                                                         {
                                                             well.ReservoirData.Clear();

                                                             foreach(var wellBoreFormation in wellBoreTechnicalDataFormsFileDate.WellBoreFormation)
                                                             {
                                                                 well.ReservoirData.Add(new ReservoirData(wellBoreFormation.WB_FORMATION_NAME, wellBoreFormation.WB_FORMATION_DEPTH));
                                                             }
                                                         }
                                                     }
                                                 }
                                             }
                                         }
                                         //Console.WriteLine($"{well.Api}");

                                         _context.Update(well);
                                     }

                                     if((i - row.Item1) % 100 == 0)
                                     {
                                         Console.WriteLine($"{threadId}: {i - row.Item1}");
                                     }
                                 }

                                 twc.Connection.Close();

                                 _context.Commit();
                                 _context.CloseConnection();
                             });
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static void LoadTexasDbs3(ImmutableList<ApiNumber> apis,
                                         int                      districtNumber)
        {
            loadTexasDbs3(apis, districtNumber);
        }

        /// <summary>
        /// Location
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private static void loadTexasDbs3(ImmutableList<ApiNumber> apis,
                                          int                      districtNumber)
        {
            Parallel.ForEach(Partitioner.Create(0, apis.Count, apis.Count / Environment.ProcessorCount),
                             row =>
                             {
                                 int threadId = System.Threading.Thread.CurrentThread.ManagedThreadId;

                                 using RrcTexasDataAdapter _context = new RrcTexasDataAdapter();

                                 using TexasDumpDbContext tdc = new TexasDumpDbContext();
                                 tdc.ChangeTracker.AutoDetectChangesEnabled = false;

                                 using TexasAggregateContext tac = new TexasAggregateContext();
                                 tac.ChangeTracker.AutoDetectChangesEnabled = false;

                                 WellLocationAggr? wellLocation;
                                 WellS?            wellSLocation;

                                 double? SURFACE_LONG27;
                                 double? SURFACE_LAT27;
                                 double? SURFACE_LONG83;
                                 double? SURFACE_LAT83;
                                 double? BOTTOM_LONG27;
                                 double? BOTTOM_LAT27;
                                 double? BOTTOM_LONG83;
                                 double? BOTTOM_LAT83;

                                 Well well;

                                 for(int i = row.Item1; i < row.Item2; i++)
                                 {
                                     well = _context.GetWellByApi(apis[i]);

                                     if(well.Location is null)
                                     {
                                         wellLocation = tac.WellLocationTable.FirstOrDefault(w => w.Api == well.Api);

                                         // lateralLengthMaybe = null;

                                         if(wellLocation != null)
                                         {
                                             SURFACE_LONG27 = wellLocation.SURFACE_LONG27;
                                             SURFACE_LAT27  = wellLocation.SURFACE_LAT27;
                                             SURFACE_LONG83 = wellLocation.SURFACE_LONG83;
                                             SURFACE_LAT83  = wellLocation.SURFACE_LAT83;
                                             BOTTOM_LONG27  = wellLocation.BOTTOM_LONG27;
                                             BOTTOM_LAT27   = wellLocation.BOTTOM_LAT27;
                                             BOTTOM_LONG83  = wellLocation.BOTTOM_LONG83;
                                             BOTTOM_LAT83   = wellLocation.BOTTOM_LAT83;

                                             _context.Add(new ShapeFileLocation(well.Api,
                                                                                SURFACE_LAT27,
                                                                                SURFACE_LONG27,
                                                                                BOTTOM_LAT27,
                                                                                BOTTOM_LONG27,
                                                                                SURFACE_LAT83,
                                                                                SURFACE_LONG83,
                                                                                BOTTOM_LAT83,
                                                                                BOTTOM_LONG83));

                                             well.Location = new Location(SURFACE_LAT27, SURFACE_LONG27, SURFACE_LAT83, SURFACE_LONG83, CountyType.GetName(well.Api.GetCountyCode()), "TEXAS")
                                             {
                                                 DistrictNumber = (short)districtNumber
                                             };

                                             // if(BOTTOM_LAT27 != null && BOTTOM_LONG27 != null)
                                             // {
                                             //     lateralLengthMaybe = new WebMercator(BOTTOM_LAT27.Value,
                                             //     BOTTOM_LONG27.Value) - new WebMercator(SURFACE_LAT27.Value,
                                             //     SURFACE_LONG27.Value); lateralLengthMaybe =
                                             //     (lateralLengthMaybe.Value > 1000) ? lateralLengthMaybe :
                                             //     null;
                                             // }

                                             //Console.WriteLine($"{well.Api}");

                                             _context.Update(well);
                                         }
                                         else
                                         {
                                             wellSLocation = tdc.WellSTable.FirstOrDefault(w => w.API == well.Api.ToShortString());

                                             if(wellSLocation != null)
                                             {
                                                 SURFACE_LONG27 = wellSLocation.LONG27;
                                                 SURFACE_LAT27  = wellSLocation.LAT27;
                                                 SURFACE_LONG83 = wellSLocation.LONG83;
                                                 SURFACE_LAT83  = wellSLocation.LAT83;

                                                 well.Location = new Location(SURFACE_LAT27,
                                                                              SURFACE_LONG27,
                                                                              SURFACE_LAT83,
                                                                              SURFACE_LONG83,
                                                                              CountyType.GetName(well.Api.GetCountyCode()),
                                                                              "TEXAS")
                                                 {
                                                     DistrictNumber = (short)districtNumber
                                                 };

                                                 _context.Update(well);
                                             }
                                         }
                                     }

                                     if((i - row.Item1) % 100 == 0)
                                     {
                                         Console.WriteLine($"{threadId}: {i - row.Item1}");
                                     }
                                 }

                                 tac.Connection.Close();

                                 _context.Commit();
                                 _context.CloseConnection();
                             });
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static void LoadAllLocations()
        {
            ImmutableList<ApiNumber> apis;

            using(TexasDumpDbContext tdc = new TexasDumpDbContext())
            {
                tdc.ChangeTracker.AutoDetectChangesEnabled = false;
                apis                                       = tdc.WellSTable.Select(w => new ApiNumber(w.API)).ToImmutableList();
                tdc.Connection.Close();
            }

            loadAllLocations(apis);
        }

        /// <summary>
        /// Location
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private static void loadAllLocations(ImmutableList<ApiNumber> apis)
        {
            ImmutableList<WellS> WellSList;
            ImmutableList<WellB> WellBList;

            using(TexasDumpDbContext tdc = new TexasDumpDbContext())
            {
                tdc.ChangeTracker.AutoDetectChangesEnabled = false;
                WellSList                                  = tdc.WellSTable.ToImmutableList();
                WellBList                                  = tdc.WellBTable.ToImmutableList();
                tdc.Connection.Close();
            }

            Console.WriteLine("Building WellS");

            Dictionary<ApiNumber, WellS> WellSDictionary = new Dictionary<ApiNumber, WellS>(WellSList.Count);

            foreach(WellS wellS in WellSList)
            {
                if(!WellSDictionary.ContainsKey(wellS.API))
                {
                    WellSDictionary.Add(new ApiNumber(wellS.API), wellS);
                }
                else if(WellSDictionary[wellS.API].Id < wellS.Id)
                {
                    WellSDictionary[wellS.API] = wellS;
                }
            }

            WellSList.Clear();

            ImmutableDictionary<ApiNumber, WellS> WellSs = WellSDictionary.ToImmutableDictionary();

            WellSDictionary.Clear();

            Console.WriteLine("Building WellB");

            Dictionary<ApiNumber, WellB> WellBDictionary = new Dictionary<ApiNumber, WellB>(WellBList.Count);

            foreach(WellB wellB in WellBList)
            {
                if(!WellBDictionary.ContainsKey(wellB.API))
                {
                    WellBDictionary.Add(new ApiNumber(wellB.API), wellB);
                }
                else if(WellBDictionary[wellB.API].Id < wellB.Id)
                {
                    WellBDictionary[wellB.API] = wellB;
                }
            }

            WellBList.Clear();

            ImmutableDictionary<ApiNumber, WellB> WellBs = WellBDictionary.ToImmutableDictionary();

            WellBDictionary.Clear();

            Console.WriteLine("Building ShapeFileLocation");

            ImmutableList<ShapeFileLocation> ShapeFileLocationList;

            using(RrcTexasDataAdapter _context = new RrcTexasDataAdapter())
            {
                _context.Context.ChangeTracker.AutoDetectChangesEnabled = false;
                ShapeFileLocationList                                   = _context.Context.ShapeFileLocations.ToImmutableList();
            }

            Dictionary<ApiNumber, ShapeFileLocation> ShapeFileLocationDictionary = new Dictionary<ApiNumber, ShapeFileLocation>(ShapeFileLocationList.Count);

            foreach(ShapeFileLocation shapeFileLocation in ShapeFileLocationList)
            {
                ShapeFileLocationDictionary.Add(new ApiNumber(shapeFileLocation.Api), shapeFileLocation);
            }

            ShapeFileLocationList.Clear();

            ImmutableDictionary<ApiNumber, ShapeFileLocation> ShapeFileLocations = ShapeFileLocationDictionary.ToImmutableDictionary();

            ShapeFileLocationDictionary.Clear();

            Console.WriteLine("Loading all Locations");

            Parallel.ForEach(Partitioner.Create(0, apis.Count, apis.Count / Environment.ProcessorCount),
                             row =>
                             {
                                 int threadId = System.Threading.Thread.CurrentThread.ManagedThreadId;

                                 using RrcTexasDataAdapter _context = new RrcTexasDataAdapter();

                                 double? SURFACE_LONG27;
                                 double? SURFACE_LAT27;
                                 double? SURFACE_LONG83;
                                 double? SURFACE_LAT83;
                                 double? BOTTOM_LONG27;
                                 double? BOTTOM_LAT27;
                                 double? BOTTOM_LONG83;
                                 double? BOTTOM_LAT83;

                                 bool               newLocation;
                                 ShapeFileLocation? shapeFileLocation;
                                 WellS?             wellSLocation;

                                 for(int i = row.Item1; i < row.Item2; i++)
                                 {
                                     if(!ShapeFileLocations.ContainsKey(apis[i]))
                                     {
                                         newLocation    = false;
                                         SURFACE_LONG27 = null;
                                         SURFACE_LAT27  = null;
                                         SURFACE_LONG83 = null;
                                         SURFACE_LAT83  = null;

                                         wellSLocation = WellSs[apis[i]];

                                         // lateralLengthMaybe = null;

                                         if(!(wellSLocation is null))
                                         {
                                             SURFACE_LONG27 = wellSLocation.LONG27;
                                             SURFACE_LAT27  = wellSLocation.LAT27;
                                             SURFACE_LONG83 = wellSLocation.LONG83;
                                             SURFACE_LAT83  = wellSLocation.LAT83;

                                             newLocation = true;
                                         }

                                         if(WellBs.TryGetValue(apis[i], out WellB wellBLocation))
                                         {
                                             BOTTOM_LONG27 = wellBLocation.LONG27;
                                             BOTTOM_LAT27  = wellBLocation.LAT27;
                                             BOTTOM_LONG83 = wellBLocation.LONG83;
                                             BOTTOM_LAT83  = wellBLocation.LAT83;
                                         }
                                         else
                                         {
                                             BOTTOM_LONG27 = 0.0;
                                             BOTTOM_LAT27  = 0.0;
                                             BOTTOM_LONG83 = 0.0;
                                             BOTTOM_LAT83  = 0.0;
                                         }

                                         if(newLocation)
                                         {
                                             shapeFileLocation = new ShapeFileLocation(apis[i],
                                                                                       SURFACE_LAT27,
                                                                                       SURFACE_LONG27,
                                                                                       BOTTOM_LAT27,
                                                                                       BOTTOM_LONG27,
                                                                                       SURFACE_LAT83,
                                                                                       SURFACE_LONG83,
                                                                                       BOTTOM_LAT83,
                                                                                       BOTTOM_LONG83);

                                             //Console.WriteLine(shapeFileLocation);

                                             _context.Add(shapeFileLocation);
                                         }
                                     }
                                     else
                                     {
                                         shapeFileLocation = ShapeFileLocations[apis[i]];

                                         if(shapeFileLocation.BottomLatitude27  == 0.0 ||
                                            shapeFileLocation.BottomLongitude27 == 0.0 ||
                                            shapeFileLocation.BottomLatitude83  == 0.0 ||
                                            shapeFileLocation.BottomLongitude83 == 0.0)
                                         {
                                             _context.Context.ShapeFileLocations.Update(new ShapeFileLocation(shapeFileLocation.Id,
                                                                                                              shapeFileLocation.Api,
                                                                                                              shapeFileLocation.SurfaceLatitude27,
                                                                                                              shapeFileLocation.SurfaceLongitude27,
                                                                                                              shapeFileLocation.SurfaceLatitude27,
                                                                                                              shapeFileLocation.SurfaceLongitude27,
                                                                                                              shapeFileLocation.SurfaceLatitude83,
                                                                                                              shapeFileLocation.SurfaceLongitude83,
                                                                                                              shapeFileLocation.SurfaceLatitude83,
                                                                                                              shapeFileLocation.SurfaceLongitude83));
                                         }
                                     }

                                     if((i - row.Item1) % 100 == 0)
                                     {
                                         Console.WriteLine($"{threadId}: {i - row.Item1}");
                                     }
                                 }

                                 _context.Commit();
                                 _context.CloseConnection();
                             });
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static void FixLocations()
        {
            Console.WriteLine("Building ShapeFileLocation");

            ImmutableList<ShapeFileLocation> ShapeFileLocationList;

            using(RrcTexasDataAdapter _context = new RrcTexasDataAdapter())
            {
                _context.Context.ChangeTracker.AutoDetectChangesEnabled = false;
                ShapeFileLocationList                                   = _context.Context.ShapeFileLocations.ToImmutableList();
            }

            Console.WriteLine("Loading all Locations");

            Parallel.ForEach(Partitioner.Create(0, ShapeFileLocationList.Count, ShapeFileLocationList.Count / Environment.ProcessorCount),
                             row =>
                             {
                                 int threadId = System.Threading.Thread.CurrentThread.ManagedThreadId;

                                 using RrcTexasDataAdapter _context = new RrcTexasDataAdapter();

                                 ShapeFileLocation shapeFileLocation;

                                 for(int i = row.Item1; i < row.Item2; i++)
                                 {
                                     shapeFileLocation = ShapeFileLocationList[i];

                                     if(shapeFileLocation.BottomLatitude27  == 0.0 ||
                                        shapeFileLocation.BottomLongitude27 == 0.0 ||
                                        shapeFileLocation.BottomLatitude83  == 0.0 ||
                                        shapeFileLocation.BottomLongitude83 == 0.0)
                                     {
                                         _context.Context.ShapeFileLocations.Update(new ShapeFileLocation(shapeFileLocation.Id,
                                                                                                          shapeFileLocation.Api,
                                                                                                          shapeFileLocation.SurfaceLatitude27,
                                                                                                          shapeFileLocation.SurfaceLongitude27,
                                                                                                          shapeFileLocation.SurfaceLatitude27,
                                                                                                          shapeFileLocation.SurfaceLongitude27,
                                                                                                          shapeFileLocation.SurfaceLatitude83,
                                                                                                          shapeFileLocation.SurfaceLongitude83,
                                                                                                          shapeFileLocation.SurfaceLatitude83,
                                                                                                          shapeFileLocation.SurfaceLongitude83));
                                     }

                                     if((i - row.Item1) % 100 == 0)
                                     {
                                         Console.WriteLine($"{threadId}: {i - row.Item1}");
                                     }
                                 }

                                 _context.Commit();
                                 _context.CloseConnection();
                             });
        }

        #region Pvt Methods

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private static void oilFieldPvt(ref ReservoirData? reservoirData,
                                        in  double?        FL_OIL_DISC_WELL_GRAVITY,
                                        in  double?        FL_G_1_GAS_GRAVITY,
                                        in  double?        FL_AVG_RESERVOIR_BHP,
                                        in  double?        FL_AVG_RESERVOIR_BH_TEMP,
                                        in  double?        FL_FORMATION_VOLUME_FACTOR,
                                        in  double?        FL_SOLUTION_GAS_OIL_RATIO)
        {
            if(reservoirData is null)
            {
                return;
            }

            if(FL_OIL_DISC_WELL_GRAVITY.IsNullOrZero())
            {
                return;
            }

            reservoirData.OilProperties ??= new OilProperties();

            if(reservoirData.OilProperties.Density.IsNullOrLessThanZero())
            {
                if(FL_OIL_DISC_WELL_GRAVITY.IsNotNullOrLessThanZero())
                {
                    reservoirData.OilProperties.Density = FL_OIL_DISC_WELL_GRAVITY;
                }
            }

            if(FL_G_1_GAS_GRAVITY.IsNullOrZero() || FL_AVG_RESERVOIR_BHP.IsNullOrZero() || FL_AVG_RESERVOIR_BH_TEMP.IsNullOrZero())
            {
                return;
            }

            double Rs = FL_SOLUTION_GAS_OIL_RATIO ??
                        Pvt.GasSolubility.VasquezBeggs(FL_AVG_RESERVOIR_BHP!.Value, FL_AVG_RESERVOIR_BH_TEMP!.Value, FL_OIL_DISC_WELL_GRAVITY!.Value, FL_G_1_GAS_GRAVITY!.Value);

            double OildeadVisocity = Pvt.OilViscosity.Dead.BeggsRobinson(FL_AVG_RESERVOIR_BH_TEMP!.Value, FL_OIL_DISC_WELL_GRAVITY!.Value);

            double Oilvisocity = Pvt.OilViscosity.Saturated.BeggsRobinson(OildeadVisocity, Rs);

            double OilformationVolumeFactor = FL_FORMATION_VOLUME_FACTOR ??
                                              Pvt.OilFormationVolumeFactor.VasquezBeggs(Rs, FL_AVG_RESERVOIR_BH_TEMP!.Value, FL_OIL_DISC_WELL_GRAVITY!.Value, FL_G_1_GAS_GRAVITY!.Value);

            double Oilcompressibility =
                Pvt.OilCompressibility.VasquezBeggs(Rs, FL_AVG_RESERVOIR_BHP!.Value, FL_AVG_RESERVOIR_BH_TEMP!.Value, FL_OIL_DISC_WELL_GRAVITY!.Value, FL_G_1_GAS_GRAVITY!.Value);

            if(reservoirData.OilProperties.FormationVolumeFactor.IsNullOrLessThanZero())
            {
                if(OilformationVolumeFactor.IsNotZero())
                {
                    reservoirData.OilProperties.FormationVolumeFactor = OilformationVolumeFactor;
                }
            }

            if(reservoirData.OilProperties.Visocity.IsNullOrLessThanZero())
            {
                if(Oilvisocity.IsNotLessThanZero())
                {
                    reservoirData.OilProperties.Visocity = Oilvisocity;
                }
            }

            if(reservoirData.OilProperties.Compressibility.IsNullOrLessThanZero())
            {
                if(Oilcompressibility.IsNotLessThanZero())
                {
                    reservoirData.OilProperties.Compressibility = Oilcompressibility;
                }
            }

            if(reservoirData.OilProperties.ReferenceTemperature.IsNullOrLessThanZero())
            {
                if(FL_AVG_RESERVOIR_BH_TEMP.IsNotNullOrLessThanZero())
                {
                    reservoirData.OilProperties.ReferenceTemperature = FL_AVG_RESERVOIR_BH_TEMP;
                }
            }

            if(reservoirData.OilProperties.ReferencePressure.IsNullOrLessThanZero())
            {
                if(FL_AVG_RESERVOIR_BHP.IsNotNullOrLessThanZero())
                {
                    reservoirData.OilProperties.ReferencePressure = FL_AVG_RESERVOIR_BHP;
                }
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private static void oilLeasePvt(ref ReservoirData? reservoirData,
                                        in  double?        AVG_RESERVOIR_BHP,
                                        in  double?        AVG_RESERVOIR_BH_TEMP,
                                        in  double?        FORMATION_VOLUME_FACTOR,
                                        in  double?        GAS_GRAVITY,
                                        in  double?        OIL_GRAVITY,
                                        in  double?        SOLUTION_GAS_OIL_RATIO)
        {
            if(reservoirData is null)
            {
                return;
            }

            if(OIL_GRAVITY.IsNullOrZero())
            {
                return;
            }

            reservoirData.OilProperties ??= new OilProperties();

            if(reservoirData.OilProperties.Density.IsNullOrLessThanZero())
            {
                if(OIL_GRAVITY.IsNotNullOrLessThanZero())
                {
                    reservoirData.OilProperties.Density = OIL_GRAVITY;
                }
            }

            if(AVG_RESERVOIR_BHP.IsNullOrZero() || AVG_RESERVOIR_BH_TEMP.IsNullOrZero() || GAS_GRAVITY.IsNullOrZero())
            {
                return;
            }

            double Rs = SOLUTION_GAS_OIL_RATIO ?? Pvt.GasSolubility.VasquezBeggs(AVG_RESERVOIR_BHP!.Value, AVG_RESERVOIR_BH_TEMP!.Value, OIL_GRAVITY!.Value, GAS_GRAVITY!.Value);

            double OildeadVisocity = Pvt.OilViscosity.Dead.BeggsRobinson(AVG_RESERVOIR_BH_TEMP!.Value, OIL_GRAVITY!.Value);

            double Oilvisocity = Pvt.OilViscosity.Saturated.BeggsRobinson(OildeadVisocity, Rs);

            double OilformationVolumeFactor = FORMATION_VOLUME_FACTOR ?? Pvt.OilFormationVolumeFactor.VasquezBeggs(Rs, AVG_RESERVOIR_BH_TEMP!.Value, OIL_GRAVITY!.Value, GAS_GRAVITY!.Value);

            double Oilcompressibility = Pvt.OilCompressibility.VasquezBeggs(Rs, AVG_RESERVOIR_BHP!.Value, AVG_RESERVOIR_BH_TEMP!.Value, OIL_GRAVITY!.Value, GAS_GRAVITY!.Value);

            if(reservoirData.OilProperties.FormationVolumeFactor.IsNullOrLessThanZero())
            {
                if(OilformationVolumeFactor.IsNotLessThanZero())
                {
                    reservoirData.OilProperties.FormationVolumeFactor = OilformationVolumeFactor;
                }
            }

            if(reservoirData.OilProperties.Visocity.IsNullOrLessThanZero())
            {
                if(Oilvisocity.IsNotLessThanZero())
                {
                    reservoirData.OilProperties.Visocity = Oilvisocity;
                }
            }

            if(reservoirData.OilProperties.Compressibility.IsNullOrLessThanZero())
            {
                if(Oilcompressibility.IsNotLessThanZero())
                {
                    reservoirData.OilProperties.Compressibility = Oilcompressibility;
                }
            }

            if(reservoirData.OilProperties.ReferenceTemperature.IsNullOrLessThanZero())
            {
                if(AVG_RESERVOIR_BH_TEMP.IsNotNullOrLessThanZero())
                {
                    reservoirData.OilProperties.ReferenceTemperature = AVG_RESERVOIR_BH_TEMP;
                }
            }

            if(reservoirData.OilProperties.ReferencePressure.IsNullOrLessThanZero())
            {
                if(AVG_RESERVOIR_BHP.IsNotNullOrLessThanZero())
                {
                    reservoirData.OilProperties.ReferencePressure = AVG_RESERVOIR_BHP;
                }
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private static void gasFieldPvt(ref ReservoirData? reservoirData,
                                        double?            FL_G_1_GAS_GRAVITY,
                                        double?            FL_AVG_RESERVOIR_BHP,
                                        double?            FL_AVG_RESERVOIR_BH_TEMP)
        {
            if(reservoirData is null)
            {
                return;
            }

            if(FL_G_1_GAS_GRAVITY.IsNullOrZero())
            {
                return;
            }

            reservoirData.GasProperties ??= new GasProperties();

            if(reservoirData.GasProperties.SpecificGravity.IsNullOrLessThanZero())
            {
                if(FL_G_1_GAS_GRAVITY.IsNotNullOrLessThanZero())
                {
                    reservoirData.GasProperties.SpecificGravity = FL_G_1_GAS_GRAVITY;
                }
            }

            if(FL_AVG_RESERVOIR_BHP.IsNullOrZero() || FL_AVG_RESERVOIR_BH_TEMP.IsNullOrZero())
            {
                return;
            }

            double Gasvisocity        = Pvt.GasViscosity.CarrKobayashiBurrows(FL_AVG_RESERVOIR_BHP!.Value, FL_AVG_RESERVOIR_BH_TEMP!.Value, FL_G_1_GAS_GRAVITY!.Value);
            double Gascompressibility = Pvt.GasCompressibility.ZFactor(FL_AVG_RESERVOIR_BHP!.Value, FL_AVG_RESERVOIR_BH_TEMP!.Value, FL_G_1_GAS_GRAVITY!.Value);

            if(reservoirData.GasProperties.Visocity.IsNullOrLessThanZero())
            {
                if(Gasvisocity.IsNotLessThanZero())
                {
                    reservoirData.GasProperties.Visocity = Gasvisocity;
                }
            }

            if(reservoirData.GasProperties.Compressibility.IsNullOrLessThanZero())
            {
                if(Gascompressibility.IsNotLessThanZero())
                {
                    reservoirData.GasProperties.Compressibility = Gascompressibility;
                }
            }

            if(reservoirData.GasProperties.ReferenceTemperature.IsNullOrLessThanZero())
            {
                if(FL_AVG_RESERVOIR_BH_TEMP.IsNotNullOrLessThanZero())
                {
                    reservoirData.GasProperties.ReferenceTemperature = FL_AVG_RESERVOIR_BH_TEMP;
                }
            }

            if(reservoirData.GasProperties.ReferencePressure.IsNullOrLessThanZero())
            {
                if(FL_AVG_RESERVOIR_BHP.IsNotNullOrLessThanZero())
                {
                    reservoirData.GasProperties.ReferencePressure = FL_AVG_RESERVOIR_BHP;
                }
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private static void gasLeasePvt(ref ReservoirData? reservoirData,
                                        double?            GAS_GRAVITY,
                                        double?            AVG_RESERVOIR_BHP,
                                        double?            AVG_RESERVOIR_BH_TEMP)
        {
            if(reservoirData is null)
            {
                return;
            }

            if(GAS_GRAVITY.IsNullOrZero())
            {
                return;
            }

            reservoirData.GasProperties ??= new GasProperties();

            if(reservoirData.GasProperties.SpecificGravity.IsNullOrLessThanZero())
            {
                if(GAS_GRAVITY.IsNotNullOrLessThanZero())
                {
                    reservoirData.GasProperties.SpecificGravity = GAS_GRAVITY;
                }
            }

            if(AVG_RESERVOIR_BHP.IsNullOrZero() || AVG_RESERVOIR_BH_TEMP.IsNullOrZero())
            {
                return;
            }

            double Gasvisocity        = Pvt.GasViscosity.CarrKobayashiBurrows(AVG_RESERVOIR_BHP!.Value, AVG_RESERVOIR_BH_TEMP!.Value, GAS_GRAVITY!.Value);
            double Gascompressibility = Pvt.GasCompressibility.ZFactor(AVG_RESERVOIR_BHP!.Value, AVG_RESERVOIR_BH_TEMP!.Value, GAS_GRAVITY!.Value);

            if(reservoirData.GasProperties.Visocity.IsNullOrLessThanZero())
            {
                if(Gasvisocity.IsNotLessThanZero())
                {
                    reservoirData.GasProperties.Visocity = Gasvisocity;
                }
            }

            if(reservoirData.GasProperties.Compressibility.IsNullOrLessThanZero())
            {
                if(Gascompressibility.IsNotLessThanZero())
                {
                    reservoirData.GasProperties.Compressibility = Gascompressibility;
                }
            }

            if(reservoirData.GasProperties.ReferenceTemperature.IsNullOrLessThanZero())
            {
                if(AVG_RESERVOIR_BH_TEMP.IsNotNullOrLessThanZero())
                {
                    reservoirData.GasProperties.ReferenceTemperature = AVG_RESERVOIR_BH_TEMP;
                }
            }

            if(reservoirData.GasProperties.ReferencePressure.IsNullOrLessThanZero())
            {
                if(AVG_RESERVOIR_BHP.IsNotNullOrLessThanZero())
                {
                    reservoirData.GasProperties.ReferencePressure = AVG_RESERVOIR_BHP;
                }
            }
        }

        #endregion

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static void LoadTexasDbs4(ImmutableList<ApiNumber> apis,
                                         int                      districtNumber)
        {
            loadTexasDb4(apis, districtNumber);
        }

        /// <summary>
        /// FieldPvt
        /// </summary>
        /// <param name="apis"></param>
        /// <param name="districtNumber"></param>
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private static void loadTexasDb4(ImmutableList<ApiNumber> apis,
                                         int                      districtNumber)
        {
            Parallel.ForEach(Partitioner.Create(0, apis.Count, apis.Count / Environment.ProcessorCount),
                             row =>
                             {
                                 int threadId = System.Threading.Thread.CurrentThread.ManagedThreadId;

                                 using RrcTexasDataAdapter _context = new RrcTexasDataAdapter();

                                 using TexasAggregateContext tac = new TexasAggregateContext();
                                 tac.ChangeTracker.AutoDetectChangesEnabled = false;

                                 WellTestAggr? wellTest;

                                 ReservoirData? reservoirData;

                                 Well well;

                                 double? maxDepth;

                                 for(int i = row.Item1; i < row.Item2; i++)
                                 {
                                     well = _context.GetWellByApi(apis[i]);

                                     if(well.Lease == null || well.Lease.Number == 0)
                                     {
                                         continue;
                                     }

                                     wellTest = tac.WellTestAggrTable.FirstOrDefault(w => w.FL_DISTRICT == districtNumber && w.FL_RRCID_DETERMINING_WELL == well.Lease.Number);

                                     if(wellTest != null)
                                     {
                                         // if(well.WellboreDetails != null && FL_AVG_RESERVOIR_BHP
                                         // is null)
                                         //{
                                         //    FL_AVG_RESERVOIR_BHP = well.WellboreDetails.TotalDepth
                                         //    * 0.65;
                                         //}

                                         if(well.ReservoirData.Count == 0)
                                         {
                                             reservoirData = new ReservoirData(wellTest.FL_RESERVOIR_NAME ?? "Unknown");
                                             well.ReservoirData.Add(reservoirData);
                                         }
                                         else
                                         {
                                             maxDepth = well.ReservoirData.Where(d => d.ReservoirDepth != null).Max(d => d.ReservoirDepth);

                                             if(maxDepth is null)
                                             {
                                                 reservoirData = well.ReservoirData.FirstOrDefault() ?? new ReservoirData("Unknown");
                                             }
                                             else
                                             {
                                                 reservoirData = well.ReservoirData.FirstOrDefault(r => r.ReservoirDepth == maxDepth) ?? new ReservoirData("Unknown");
                                             }
                                         }

                                         oilFieldPvt(ref reservoirData,
                                                     wellTest.FL_OIL_DISC_WELL_GRAVITY,
                                                     wellTest.FL_G_1_GAS_GRAVITY,
                                                     wellTest.FL_AVG_RESERVOIR_BHP,
                                                     wellTest.FL_AVG_RESERVOIR_BH_TEMP,
                                                     wellTest.FL_FORMATION_VOLUME_FACTOR,
                                                     wellTest.FL_SOLUTION_GAS_OIL_RATIO);

                                         gasFieldPvt(ref reservoirData, wellTest.FL_G_1_GAS_GRAVITY, wellTest.FL_AVG_RESERVOIR_BHP, wellTest.FL_AVG_RESERVOIR_BH_TEMP);

                                         if(reservoirData != null)
                                         {
                                             reservoirData.ReservoirProperties = new ReservoirProperties
                                             {
                                                 InitialPressure = wellTest.FL_AVG_RESERVOIR_BHP.IsNotNullOrZero() ? wellTest.FL_AVG_RESERVOIR_BHP : null,
                                                 Temperature     = wellTest.FL_AVG_RESERVOIR_BH_TEMP.IsNotNullOrZero() ? wellTest.FL_AVG_RESERVOIR_BH_TEMP : null
                                             };
                                         }

                                         Console.WriteLine($"{well.Api}");

                                         _context.Update(well);
                                     }

                                     if((i - row.Item1) % 100 == 0)
                                     {
                                         Console.WriteLine($"{threadId}: {i - row.Item1}");
                                     }
                                 }

                                 tac.Connection.Close();

                                 _context.Commit();
                                 _context.CloseConnection();
                             });
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static void LoadTexasDbs5(ImmutableList<LeaseTestAggr> leaseTests)
        {
            loadTexasDbs5(leaseTests);
        }

        /// <summary>
        /// LeasePvt
        /// </summary>
        /// <param name="apis"></param>
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private static void loadTexasDbs5(ImmutableList<LeaseTestAggr> leaseTests)
        {
            Parallel.ForEach(Partitioner.Create(0, leaseTests.Count, leaseTests.Count / Environment.ProcessorCount),
                             row =>
                             {
                                 int threadId = System.Threading.Thread.CurrentThread.ManagedThreadId;

                                 using RrcTexasDataAdapter _context = new RrcTexasDataAdapter();

                                 LeaseTestAggr? leaseTest;

                                 ReservoirData? reservoirData;

                                 double? maxDepth;

                                 Well well;

                                 for(int i = row.Item1; i < row.Item2; i++)
                                 {
                                     leaseTest = leaseTests[i];

                                     well = _context.GetWellByApi(leaseTest.Api);

                                     if(well != null)
                                     {
                                         if(well.ReservoirData.Count == 0)
                                         {
                                             reservoirData = new ReservoirData("Unknown");
                                             well.ReservoirData.Add(reservoirData);
                                         }
                                         else
                                         {
                                             maxDepth = well.ReservoirData.Where(d => d.ReservoirDepth != null).Max(d => d.ReservoirDepth);

                                             if(maxDepth is null)
                                             {
                                                 reservoirData = well.ReservoirData.FirstOrDefault() ?? new ReservoirData("Unknown");
                                             }
                                             else
                                             {
                                                 reservoirData = well.ReservoirData.FirstOrDefault(r => r.ReservoirDepth == maxDepth) ?? new ReservoirData("Unknown");
                                             }
                                         }

                                         oilLeasePvt(ref reservoirData,
                                                     leaseTest.AVG_RESERVOIR_BHP,
                                                     leaseTest.AVG_RESERVOIR_BH_TEMP,
                                                     leaseTest.FORMATION_VOLUME_FACTOR,
                                                     leaseTest.GAS_GRAVITY,
                                                     leaseTest.OIL_GRAVITY,
                                                     leaseTest.SOLUTION_GAS_OIL_RATIO);

                                         gasLeasePvt(ref reservoirData, leaseTest.GAS_GRAVITY, leaseTest.AVG_RESERVOIR_BHP, leaseTest.AVG_RESERVOIR_BH_TEMP);

                                         if(reservoirData != null && reservoirData.ReservoirProperties is null)
                                         {
                                             reservoirData.ReservoirProperties = new ReservoirProperties
                                             {
                                                 InitialPressure = leaseTest.AVG_RESERVOIR_BHP, Temperature = leaseTest.AVG_RESERVOIR_BH_TEMP
                                             };
                                         }
                                         else
                                         {
                                             if(reservoirData?.ReservoirProperties != null && reservoirData.ReservoirProperties.InitialPressure.IsNullOrLessThanZero())
                                             {
                                                 if(leaseTest.AVG_RESERVOIR_BHP.IsNotNullOrLessThanZero())
                                                 {
                                                     reservoirData.ReservoirProperties.InitialPressure = leaseTest.AVG_RESERVOIR_BHP;
                                                 }
                                             }

                                             if(reservoirData?.ReservoirProperties != null && reservoirData.ReservoirProperties.Temperature.IsNullOrLessThanZero())
                                             {
                                                 if(leaseTest.AVG_RESERVOIR_BH_TEMP.IsNotNullOrLessThanZero())
                                                 {
                                                     reservoirData.ReservoirProperties.Temperature = leaseTest.AVG_RESERVOIR_BH_TEMP;
                                                 }
                                             }
                                         }

                                         _context.Update(well);
                                     }

                                     if((i - row.Item1) % 100 == 0)
                                     {
                                         Console.WriteLine($"{threadId}: {i - row.Item1}");
                                     }
                                 }

                                 _context.Commit();
                                 _context.CloseConnection();
                             });
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static void LoadTexasDbs6(ImmutableList<ApiNumber> apis)
        {
            loadTexasDbs6(apis);
        }

        /// <summary>
        /// MonthlyProduction
        /// </summary>
        /// <param name="apis"></param>
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private static void loadTexasDbs6(ImmutableList<ApiNumber> apis)
        {
            Parallel.ForEach(Partitioner.Create(0, apis.Count, apis.Count / Environment.ProcessorCount),
                             row =>
                             {
                                 int threadId = System.Threading.Thread.CurrentThread.ManagedThreadId;

                                 using RrcTexasDataAdapter _context = new RrcTexasDataAdapter();

                                 using TexasAggregateContext tac = new TexasAggregateContext();
                                 tac.ChangeTracker.AutoDetectChangesEnabled = false;

                                 ImmutableList<WellProductionAggr> production;

                                 Well well;

                                 for(int i = row.Item1; i < row.Item2; i++)
                                 {
                                     well = _context.GetWellByApi(apis[i]);

                                     production = tac.WellProductionAggrTable.Where(w => w.Api == well.Api).ToImmutableList();

                                     if(production != null && production.Count > 0)
                                     {
                                         well.MonthlyProduction.Clear();

                                         foreach(WellProductionAggr monthly in production)
                                         {
                                             well.MonthlyProduction.Add(new MonthlyProduction(monthly.Date,
                                                                                              (double)(monthly.GAS_VOL  ?? 0.0),
                                                                                              (double)(monthly.COND_VOL ?? 0.0),
                                                                                              (double)(monthly.OIL_VOL  ?? 0.0),
                                                                                              0.0));
                                         }

                                         //Console.WriteLine($"{well.Api}");

                                         _context.Update(well);
                                     }

                                     if((i - row.Item1) % 100 == 0)
                                     {
                                         Console.WriteLine($"{threadId}: {i - row.Item1}");
                                     }
                                 }

                                 tac.Connection.Close();

                                 _context.Commit();
                                 _context.CloseConnection();
                             });
        }
    }
}