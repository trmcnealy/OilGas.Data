using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Data;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Security;
using System.Text.RegularExpressions;

using Engineering.DataSource;
using Engineering.DataSource.Tools;

using Kokkos;

using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Diagnostics;
using Microsoft.EntityFrameworkCore.Infrastructure;
using Microsoft.EntityFrameworkCore.Storage;
using Microsoft.Extensions.Caching.Memory;
using Microsoft.Extensions.Logging;

using Npgsql;

namespace OilGas.Data.RRC.Texas
{
    public enum DbfTape
    {
        /// Well Bore Technical Data Root
        WBROOT = 1,

        /// Well Bore Completion Information
        WBCOMPL = 2,

        /// Well Bore Technical Data Forms File
        WBDATE = 3,

        /// Well Bore Remarks
        WBRMKS = 4,

        /// Well Bore Tubing
        WBTUBE = 5,

        /// Well Bore Casing
        WBCASE = 6,

        /// Well Bore Perforations
        WBPERF = 7,

        /// Well Bore Liner
        WBLINE = 8,

        /// Well Bore Formation
        WBFORM = 9,

        /// Well Bore Squeeze
        WBSQEZE = 10,

        /// Well Bore Usable Quality Water Protection
        WBFRESH = 11,

        /// Well Bore Old Location
        WBOLDLOC = 12,

        /// Well Bore New Location
        WBNEWLOC = 13,

        /// Well Bore Plugging Data
        WBPLUG = 14,

        /// Well Bore Plugging Remarks
        WBPLRMKS = 15,

        /// Well Bore Plugging Record 
        WBPLREC = 16,

        /// Well Bore Plugging Data Casing-Tubing
        WBPLCASE = 17,

        /// Well Bore Plugging Perfs
        WBPLPERF = 18,

        /// Well Bore Plugging Data Nomenclature
        WBPLNAME = 19,

        /// Well Bore Drilling Permit Number
        WBDRILL = 20,

        /// Well Bore Well-ID
        WBWELLID = 21,

        /// 14B2 Well   
        WB14B2 = 22,

        /// H-15 Report
        WBH15 = 23,

        /// H-15 Remarks
        WBH15RMK = 24,

        /// Senate Bill 126
        WBSB126 = 25,

        /// Well Bore Drilling Permit Status
        WBDASTAT = 26,

        /// Well Bore W3C Data
        WBW3C = 27,

        /// Well Bore 14B2 Remarks
        WB14B2RM = 28
    }

    /// <summary>psql --username= --password --host=timothyrmcnealy.com --port=5432 --dbname=TexasWellbore</summary>
    public sealed class TexasWellboreContext : DbContext
    {
        public const string Host = "timothyrmcnealy.com";
        public const string Port = "15432";

        private static readonly IMemoryCache _cache = new MemoryCache(new MemoryCacheOptions());

        public DbSet<WellBoreH15Remarks> WellBoreH15RemarksTable { get; set; }

        public DbSet<WellBoreH15Report> WellBoreH15ReportTable { get; set; }

        public DbSet<WellBoreLiner> WellBoreLinerTable { get; set; }

        public DbSet<WellBoreNewLocation> WellBoreNewLocationTable { get; set; }

        public DbSet<WellBoreOldLocation> WellBoreOldLocationTable { get; set; }

        public DbSet<WellBorePerforations> WellBorePerforationsTable { get; set; }

        public DbSet<WellBorePluggingData> WellBorePluggingDataTable { get; set; }

        public DbSet<WellBorePluggingDataCasingTubing> WellBorePluggingDataCasingTubingTable { get; set; }

        public DbSet<WellBorePluggingDataNomenclature> WellBorePluggingDataNomenclatureTable { get; set; }

        public DbSet<WellBorePluggingPerfs> WellBorePluggingPerfsTable { get; set; }

        public DbSet<WellBorePluggingRecord> WellBorePluggingRecordTable { get; set; }

        public DbSet<WellBorePluggingRemarks> WellBorePluggingRemarksTable { get; set; }

        public DbSet<WellBoreRemarks> WellBoreRemarksTable { get; set; }

        public DbSet<WellBoreSenateBill126> WellBoreSenateBill126Table { get; set; }

        public DbSet<WellBoreSqueeze> WellBoreSqueezeTable { get; set; }

        public DbSet<WellBoreTechnicalDataFormsFileDate> WellBoreTechnicalDataFormsFileDateTable { get; set; }

        public DbSet<WellBoreTechnicalDataRoot> WellBoreTechnicalDataRootTable { get; set; }

        public DbSet<WellBoreTubing> WellBoreTubingTable { get; set; }

        public DbSet<WellBoreUsableQualityWaterProtection> WellBoreUsableQualityWaterProtectionTable { get; set; }

        public DbSet<WellBoreW3C> WellBoreW3CTable { get; set; }

        public DbSet<WellBoreWellId> WellBoreWellIdTable { get; set; }

        public DbSet<WellBore14B2Remarks> WellBore14B2RemarksTable { get; set; }

        public DbSet<WellBore14B2Well> WellBore14B2WellTable { get; set; }

        public DbSet<WellBoreCasing> WellBoreCasingTable { get; set; }

        public DbSet<WellBoreCompletionInformation> WellBoreCompletionInformationTable { get; set; }

        public DbSet<WellBoreDrillingPermitNumber> WellBoreDrillingPermitNumberTable { get; set; }

        public DbSet<WellBoreDrillingPermitStatus> WellBoreDrillingPermitStatusTable { get; set; }

        public DbSet<WellBoreFormation> WellBoreFormationTable { get; set; }

        public NpgsqlConnection Connection { get; }

        public TexasWellboreContext()
            : this(CreateAndOpen())
        {
        }

        public TexasWellboreContext(NpgsqlConnection connection)
            : base(new DbContextOptionsBuilder<TexasWellboreContext>().UseLazyLoadingProxies()
                                                                      .UseMemoryCache(_cache)
                                                                      .UseNpgsql(connection)
                                                                       /*.EnableSensitiveDataLogging()
                                                                       .LogTo(Console.WriteLine, LogLevel.Information)*/
                                                                      .Options)
        {
            Connection = connection;

            Database.EnsureCreated();

            Database.SetCommandTimeout(150000);
        }

        private static NpgsqlConnection CreateAndOpen()
        {
            NpgsqlConnection connection;

            try
            {
                connection = new NpgsqlConnection($"Host={Host};Port={Port};Username={Encryption.Username};Password={Encryption.Password};Database=TexasWellbore");

                connection.Open();
            }
            catch(Exception)
            {
                connection = new NpgsqlConnection($"Host={Host};Port={Port};Username=db_user;Password=dbAccess;Database=TexasWellbore");

                connection.Open();
            }

            return connection;
        }

        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            base.OnModelCreating(modelBuilder);
        }

        public void CreateTables()
        {
            RelationalDatabaseCreator databaseCreator = (RelationalDatabaseCreator)Database.GetService<IDatabaseCreator>();
            databaseCreator.CreateTables();

            SaveChanges();
        }

        public void Vacuum(string tableName)
        {
            NpgsqlCommand command = Connection.CreateCommand();

            command.CommandText = $"VACUUM(FULL) \"{tableName}\";";

            command.ExecuteNonQuery();
        }

        public void VacuumAndAnalyze(string tableName)
        {
            NpgsqlCommand command = Connection.CreateCommand();

            command.CommandText = $"VACUUM(FULL, ANALYZE) \"{tableName}\";";

            command.ExecuteNonQuery();
        }

        public void ReIndex(string tableName)
        {
            NpgsqlCommand command = Connection.CreateCommand();

            command.CommandText = $"REINDEX TABLE CONCURRENTLY \"{tableName}\";";

            command.ExecuteNonQuery();
        }

        public override void Dispose()
        {
            base.Dispose();

            if(Connection.State == ConnectionState.Open)
            {
                Connection.Close();
            }
        }

        public static IEnumerable<List<T>> Partition<T>(List<T> source,
                                                        int     size)
        {
            for(int i = 0; i < Math.Ceiling(source.Count / (double)size); ++i)
            {
                yield return new List<T>(source.Skip(size * i).Take(size));
            }
        }

        public static IEnumerable<List<T>> Partition<T>(T[] source,
                                                        int size)
        {
            for(int i = 0; i < Math.Ceiling(source.Length / (double)size); ++i)
            {
                yield return new List<T>(source.Skip(size * i).Take(size));
            }
        }

        public List<ApiNumber> ByApiCountyCode(int countyCode)
        {
            List<WellBoreTechnicalDataRoot> wellBoreTechnicalDataRoots = WellBoreTechnicalDataRootTable.Where(w => w.WB_API_CNTY == countyCode).ToList();

            List<ApiNumber> apis = new List<ApiNumber>(wellBoreTechnicalDataRoots.Count);

            foreach(WellBoreTechnicalDataRoot wellBoreTechnicalDataRoot in wellBoreTechnicalDataRoots)
            {
                if(wellBoreTechnicalDataRoot.WB_API_CNTY is null || wellBoreTechnicalDataRoot.WB_API_UNIQUE is null)
                {
                    continue;
                }

                apis.Add(new ApiNumber(42, (uint)wellBoreTechnicalDataRoot.WB_API_CNTY, (uint)wellBoreTechnicalDataRoot.WB_API_UNIQUE));
            }

            return apis;
        }

        public void LoadRelationalProperties(WellBoreTechnicalDataRoot wellBoreTechnicalDataRoot)
        {
            //Entry(wellBoreTechnicalDataRoot).Collection(nameof(WellBoreTechnicalDataRoot.WellBoreCompletionInformation)).Load();

            //if (wellBoreTechnicalDataRoot.WellBoreCompletionInformation != null)
            //{
            //    foreach (WellBoreCompletionInformation wellBoreCompletionInformation in wellBoreTechnicalDataRoot.WellBoreCompletionInformation)
            //    {
            //        Entry(wellBoreCompletionInformation).Collection(nameof(WellBoreCompletionInformation.WellBoreTechnicalDataFormsFileDate)).Load();

            //        if (wellBoreCompletionInformation.WellBoreTechnicalDataFormsFileDate != null)
            //        {
            //            foreach (WellBoreTechnicalDataFormsFileDate wellBoreTechnicalDataFormsFileDate in wellBoreCompletionInformation.WellBoreTechnicalDataFormsFileDate)
            //            {
            //                Entry(wellBoreTechnicalDataFormsFileDate).Collection(nameof(WellBoreTechnicalDataFormsFileDate.WellBoreRemarks)).Load();
            //                Entry(wellBoreTechnicalDataFormsFileDate).Collection(nameof(WellBoreTechnicalDataFormsFileDate.WellBoreTubing)).Load();
            //                Entry(wellBoreTechnicalDataFormsFileDate).Collection(nameof(WellBoreTechnicalDataFormsFileDate.WellBoreCasing)).Load();
            //                Entry(wellBoreTechnicalDataFormsFileDate).Collection(nameof(WellBoreTechnicalDataFormsFileDate.WellBorePerforations)).Load();
            //                Entry(wellBoreTechnicalDataFormsFileDate).Collection(nameof(WellBoreTechnicalDataFormsFileDate.WellBoreLiner)).Load();
            //                Entry(wellBoreTechnicalDataFormsFileDate).Collection(nameof(WellBoreTechnicalDataFormsFileDate.WellBoreFormation)).Load();
            //                Entry(wellBoreTechnicalDataFormsFileDate).Collection(nameof(WellBoreTechnicalDataFormsFileDate.WellBoreSqueeze)).Load();
            //                Entry(wellBoreTechnicalDataFormsFileDate).Collection(nameof(WellBoreTechnicalDataFormsFileDate.WellBoreUsableQualityWaterProtection)).Load();
            //            }
            //        }
            //    }
            //}

            //Entry(wellBoreTechnicalDataRoot).Reference(nameof(WellBoreTechnicalDataRoot.WellBoreOldLocation)).Load();
            //Entry(wellBoreTechnicalDataRoot).Reference(nameof(WellBoreTechnicalDataRoot.WellBoreNewLocation)).Load();
            //Entry(wellBoreTechnicalDataRoot).Collection(nameof(WellBoreTechnicalDataRoot.WellBorePluggingData)).Load();

            //if (wellBoreTechnicalDataRoot.WellBorePluggingData != null)
            //{
            //    foreach (WellBorePluggingData wellBorePluggingData in wellBoreTechnicalDataRoot.WellBorePluggingData)
            //    {
            //        Entry(wellBorePluggingData).Collection(nameof(WellBorePluggingData.WellBorePluggingRemarks)).Load();
            //        Entry(wellBorePluggingData).Reference(nameof(WellBorePluggingData.WellBorePluggingRecord)).Load();
            //        Entry(wellBorePluggingData).Collection(nameof(WellBorePluggingData.WellBorePluggingDataCasingTubing)).Load();
            //        Entry(wellBorePluggingData).Collection(nameof(WellBorePluggingData.WellBorePluggingPerfs)).Load();
            //        Entry(wellBorePluggingData).Reference(nameof(WellBorePluggingData.WellBorePluggingDataNomenclature)).Load();
            //    }
            //}

            //Entry(wellBoreTechnicalDataRoot).Collection(nameof(WellBoreTechnicalDataRoot.WellBoreDrillingPermitNumber)).Load();

            //if (wellBoreTechnicalDataRoot.WellBoreDrillingPermitNumber != null)
            //{
            //    foreach (WellBoreDrillingPermitNumber wellBoreDrillingPermitNumber in wellBoreTechnicalDataRoot.WellBoreDrillingPermitNumber)
            //    {
            //        Entry(wellBoreDrillingPermitNumber).Collection(nameof(WellBoreDrillingPermitNumber.WellBoreWellId)).Load();
            //    }
            //}

            //Entry(wellBoreTechnicalDataRoot).Reference(nameof(WellBoreTechnicalDataRoot.WellBore14B2Well)).Load();

            //if (wellBoreTechnicalDataRoot.WellBore14B2Well != null)
            //{
            //    Entry(wellBoreTechnicalDataRoot.WellBore14B2Well).Collection(nameof(WellBore14B2Well.WellBore14B2Remarks)).Load();
            //}

            //Entry(wellBoreTechnicalDataRoot).Collection(nameof(WellBoreTechnicalDataRoot.WellBoreH15Report)).Load();

            //if (wellBoreTechnicalDataRoot.WellBoreH15Report != null)
            //{
            //    foreach (WellBoreH15Report wellBoreH15Report in wellBoreTechnicalDataRoot.WellBoreH15Report)
            //    {
            //        Entry(wellBoreH15Report).Collection(nameof(WellBoreH15Report.WellBoreH15Remarks)).Load();
            //    }
            //}

            //Entry(wellBoreTechnicalDataRoot).Reference(nameof(WellBoreTechnicalDataRoot.WellBoreSenateBill126)).Load();
            //Entry(wellBoreTechnicalDataRoot).Collection(nameof(WellBoreTechnicalDataRoot.WellBoreDrillingPermitStatus)).Load();
            //Entry(wellBoreTechnicalDataRoot).Collection(nameof(WellBoreTechnicalDataRoot.WellBoreW3C)).Load();
        }

        public WellBoreTechnicalDataRoot ByApi(ApiNumber api)
        {
            WellBoreTechnicalDataRoot wellBoreTechnicalDataRoot =
                WellBoreTechnicalDataRootTable.SingleOrDefault(w => w.WB_API_CNTY == (int)api.GetCountyCode() && w.WB_API_UNIQUE == (int)api.GetUniqueWellIdentifier());

            if(wellBoreTechnicalDataRoot is null)
            {
                return null;
            }

            return wellBoreTechnicalDataRoot;
        }

        private static ApiNumber ToApiNumber(WellBoreTechnicalDataRoot w)
        {
            if(w.WB_API_CNTY is null || w.WB_API_UNIQUE is null)
            {
                return new ApiNumber();
            }

            return new ApiNumber(42, (uint)w.WB_API_CNTY, (uint)w.WB_API_UNIQUE, 0, 0);
        }

        public ImmutableList<WellBoreTechnicalDataRoot> ByApis(int districtNumber)
        {
            List<WellBoreTechnicalDataRoot> wellBoreTechnicalDataRoots = WellBoreTechnicalDataRootTable.Where(s => s.WB_FIELD_DISTRICT == districtNumber).ToList();

            return wellBoreTechnicalDataRoots.ToImmutableList();
        }

        public ImmutableList<WellBoreTechnicalDataRoot> GetAll(CountyType county)
        {
            List<WellBoreTechnicalDataRoot> wellBoreTechnicalDataRoots = WellBoreTechnicalDataRootTable.Where(w => w.WB_API_CNTY == county).ToList();
            
            return wellBoreTechnicalDataRoots.ToImmutableList();
        }

        public void LoadFieldInformation_dbf900(params string[] filePaths)
        {
            using(ScopeGuard.Get())
            {
                List<WellBoreTechnicalDataRoot> wellBoreTechnicalDataRoots = new List<WellBoreTechnicalDataRoot>(100000);
                int                             counter                    = 0;

                WellBoreH15Remarks                   wellBoreH15Remarks                   = null;
                WellBoreH15Report                    wellBoreH15Report                    = null;
                WellBoreLiner                        wellBoreLiner                        = null;
                WellBoreNewLocation                  wellBoreNewLocation                  = null;
                WellBoreOldLocation                  wellBoreOldLocation                  = null;
                WellBorePerforations                 wellBorePerforations                 = null;
                WellBorePluggingData                 wellBorePluggingData                 = null;
                WellBorePluggingDataCasingTubing     wellBorePluggingDataCasingTubing     = null;
                WellBorePluggingDataNomenclature     wellBorePluggingDataNomenclature     = null;
                WellBorePluggingPerfs                wellBorePluggingPerfs                = null;
                WellBorePluggingRecord               wellBorePluggingRecord               = null;
                WellBorePluggingRemarks              wellBorePluggingRemarks              = null;
                WellBoreRemarks                      wellBoreRemarks                      = null;
                WellBoreSenateBill126                wellBoreSenateBill126                = null;
                WellBoreSqueeze                      wellBoreSqueeze                      = null;
                WellBoreTechnicalDataFormsFileDate   wellBoreTechnicalDataFormsFileDate   = null;
                WellBoreTechnicalDataRoot            wellBoreTechnicalDataRoot            = null;
                WellBoreTubing                       wellBoreTubing                       = null;
                WellBoreUsableQualityWaterProtection wellBoreUsableQualityWaterProtection = null;
                WellBoreW3C                          wellBoreW3C                          = null;
                WellBoreWellId                       wellBoreWellId                       = null;
                WellBore14B2Remarks                  wellBore14B2Remarks                  = null;
                WellBore14B2Well                     wellBore14B2Well                     = null;
                WellBoreCasing                       wellBoreCasing                       = null;
                WellBoreCompletionInformation        wellBoreCompletionInformation        = null;
                WellBoreDrillingPermitNumber         wellBoreDrillingPermitNumber         = null;
                WellBoreDrillingPermitStatus         wellBoreDrillingPermitStatus         = null;
                WellBoreFormation                    wellBoreFormation                    = null;

                foreach(string filePath in filePaths)
                {
                    using(MemoryMapped srcMM = new MemoryMapped(filePath))
                    {
                        unsafe
                        {
                            byte* ptr = srcMM.GetPointer<byte>();

                            RecordLayoutTemplate template;

                            ulong offset = 0;

                            ReadOnlySpan<byte> str;

                            while(srcMM.Size() > offset)
                            {
                                ++counter;

                                str = new ReadOnlySpan<byte>(ptr + offset, 247);

                                template = new RecordLayoutTemplate(str);

                                switch((DbfTape)template.GetKeyValue())
                                {
                                    case DbfTape.WBROOT:
                                    {
                                        if(wellBoreTechnicalDataRoot != null)
                                        {
                                            wellBoreTechnicalDataRoots.Add(wellBoreTechnicalDataRoot);

                                            wellBoreTechnicalDataRoot = null;
                                        }

                                        wellBoreTechnicalDataRoot = new WellBoreTechnicalDataRoot(template.Record);

                                        wellBoreH15Remarks                   = null;
                                        wellBoreH15Report                    = null;
                                        wellBoreLiner                        = null;
                                        wellBoreNewLocation                  = null;
                                        wellBoreOldLocation                  = null;
                                        wellBorePerforations                 = null;
                                        wellBorePluggingData                 = null;
                                        wellBorePluggingDataCasingTubing     = null;
                                        wellBorePluggingDataNomenclature     = null;
                                        wellBorePluggingPerfs                = null;
                                        wellBorePluggingRecord               = null;
                                        wellBorePluggingRemarks              = null;
                                        wellBoreRemarks                      = null;
                                        wellBoreSenateBill126                = null;
                                        wellBoreSqueeze                      = null;
                                        wellBoreTechnicalDataFormsFileDate   = null;
                                        wellBoreTubing                       = null;
                                        wellBoreUsableQualityWaterProtection = null;
                                        wellBoreW3C                          = null;
                                        wellBoreWellId                       = null;
                                        wellBore14B2Remarks                  = null;
                                        wellBore14B2Well                     = null;
                                        wellBoreCasing                       = null;
                                        wellBoreCompletionInformation        = null;
                                        wellBoreDrillingPermitNumber         = null;
                                        wellBoreDrillingPermitStatus         = null;
                                        wellBoreFormation                    = null;

                                        break;
                                    }

                                    case DbfTape.WBCOMPL:
                                    {
                                        wellBoreCompletionInformation = new WellBoreCompletionInformation(template.Record);
                                        wellBoreTechnicalDataRoot.WellBoreCompletionInformation.Add(wellBoreCompletionInformation);

                                        break;
                                    }
                                    case DbfTape.WBDATE:
                                    {
                                        wellBoreTechnicalDataFormsFileDate = new WellBoreTechnicalDataFormsFileDate(template.Record);
                                        wellBoreCompletionInformation.WellBoreTechnicalDataFormsFileDate.Add(wellBoreTechnicalDataFormsFileDate);

                                        break;
                                    }
                                    case DbfTape.WBRMKS:
                                    {
                                        wellBoreRemarks = new WellBoreRemarks(template.Record);
                                        wellBoreTechnicalDataFormsFileDate.WellBoreRemarks.Add(wellBoreRemarks);

                                        break;
                                    }
                                    case DbfTape.WBTUBE:
                                    {
                                        wellBoreTubing = new WellBoreTubing(template.Record);
                                        wellBoreTechnicalDataFormsFileDate.WellBoreTubing.Add(wellBoreTubing);

                                        break;
                                    }
                                    case DbfTape.WBCASE:
                                    {
                                        wellBoreCasing = new WellBoreCasing(template.Record);
                                        wellBoreTechnicalDataFormsFileDate.WellBoreCasing.Add(wellBoreCasing);

                                        break;
                                    }
                                    case DbfTape.WBPERF:
                                    {
                                        wellBorePerforations = new WellBorePerforations(template.Record);
                                        wellBoreTechnicalDataFormsFileDate.WellBorePerforations.Add(wellBorePerforations);

                                        break;
                                    }
                                    case DbfTape.WBLINE:
                                    {
                                        wellBoreLiner = new WellBoreLiner(template.Record);
                                        wellBoreTechnicalDataFormsFileDate.WellBoreLiner.Add(wellBoreLiner);

                                        break;
                                    }
                                    case DbfTape.WBFORM:
                                    {
                                        wellBoreFormation = new WellBoreFormation(template.Record);
                                        wellBoreTechnicalDataFormsFileDate.WellBoreFormation.Add(wellBoreFormation);

                                        break;
                                    }
                                    case DbfTape.WBSQEZE:
                                    {
                                        wellBoreSqueeze = new WellBoreSqueeze(template.Record);
                                        wellBoreTechnicalDataFormsFileDate.WellBoreSqueeze.Add(wellBoreSqueeze);

                                        break;
                                    }
                                    case DbfTape.WBFRESH:
                                    {
                                        wellBoreUsableQualityWaterProtection = new WellBoreUsableQualityWaterProtection(template.Record);
                                        wellBoreTechnicalDataFormsFileDate.WellBoreUsableQualityWaterProtection.Add(wellBoreUsableQualityWaterProtection);

                                        break;
                                    }
                                    case DbfTape.WBOLDLOC:
                                    {
                                        wellBoreOldLocation                           = new WellBoreOldLocation(template.Record);
                                        wellBoreTechnicalDataRoot.WellBoreOldLocation = wellBoreOldLocation;

                                        break;
                                    }
                                    case DbfTape.WBNEWLOC:
                                    {
                                        wellBoreNewLocation                           = new WellBoreNewLocation(template.Record);
                                        wellBoreTechnicalDataRoot.WellBoreNewLocation = wellBoreNewLocation;

                                        break;
                                    }
                                    case DbfTape.WBPLUG:
                                    {
                                        wellBorePluggingData = new WellBorePluggingData(template.Record);
                                        wellBoreTechnicalDataRoot.WellBorePluggingData.Add(wellBorePluggingData);

                                        break;
                                    }
                                    case DbfTape.WBPLRMKS:
                                    {
                                        wellBorePluggingRemarks = new WellBorePluggingRemarks(template.Record);
                                        wellBorePluggingData.WellBorePluggingRemarks.Add(wellBorePluggingRemarks);

                                        break;
                                    }
                                    case DbfTape.WBPLREC:
                                    {
                                        wellBorePluggingRecord                      = new WellBorePluggingRecord(template.Record);
                                        wellBorePluggingData.WellBorePluggingRecord = wellBorePluggingRecord;

                                        break;
                                    }
                                    case DbfTape.WBPLCASE:
                                    {
                                        wellBorePluggingDataCasingTubing = new WellBorePluggingDataCasingTubing(template.Record);
                                        wellBorePluggingData.WellBorePluggingDataCasingTubing.Add(wellBorePluggingDataCasingTubing);

                                        break;
                                    }
                                    case DbfTape.WBPLPERF:
                                    {
                                        wellBorePluggingPerfs = new WellBorePluggingPerfs(template.Record);
                                        wellBorePluggingData.WellBorePluggingPerfs.Add(wellBorePluggingPerfs);

                                        break;
                                    }
                                    case DbfTape.WBPLNAME:
                                    {
                                        wellBorePluggingDataNomenclature                      = new WellBorePluggingDataNomenclature(template.Record);
                                        wellBorePluggingData.WellBorePluggingDataNomenclature = wellBorePluggingDataNomenclature;

                                        break;
                                    }
                                    case DbfTape.WBDRILL:
                                    {
                                        wellBoreDrillingPermitNumber = new WellBoreDrillingPermitNumber(template.Record);
                                        wellBoreTechnicalDataRoot.WellBoreDrillingPermitNumber.Add(wellBoreDrillingPermitNumber);

                                        break;
                                    }
                                    case DbfTape.WBWELLID:
                                    {
                                        wellBoreWellId = new WellBoreWellId(template.Record);
                                        wellBoreDrillingPermitNumber.WellBoreWellId.Add(wellBoreWellId);

                                        break;
                                    }
                                    case DbfTape.WB14B2:
                                    {
                                        wellBore14B2Well                           = new WellBore14B2Well(template.Record);
                                        wellBoreTechnicalDataRoot.WellBore14B2Well = wellBore14B2Well;

                                        break;
                                    }
                                    case DbfTape.WBH15:
                                    {
                                        wellBoreH15Report = new WellBoreH15Report(template.Record);
                                        wellBoreTechnicalDataRoot.WellBoreH15Report.Add(wellBoreH15Report);

                                        break;
                                    }
                                    case DbfTape.WBH15RMK:
                                    {
                                        wellBoreH15Remarks = new WellBoreH15Remarks(template.Record);
                                        wellBoreH15Report.WellBoreH15Remarks.Add(wellBoreH15Remarks);

                                        break;
                                    }
                                    case DbfTape.WBSB126:
                                    {
                                        wellBoreSenateBill126                           = new WellBoreSenateBill126(template.Record);
                                        wellBoreTechnicalDataRoot.WellBoreSenateBill126 = wellBoreSenateBill126;

                                        break;
                                    }
                                    case DbfTape.WBDASTAT:
                                    {
                                        wellBoreDrillingPermitStatus = new WellBoreDrillingPermitStatus(template.Record);
                                        wellBoreTechnicalDataRoot.WellBoreDrillingPermitStatus.Add(wellBoreDrillingPermitStatus);

                                        break;
                                    }
                                    case DbfTape.WBW3C:
                                    {
                                        wellBoreW3C = new WellBoreW3C(template.Record);
                                        wellBoreTechnicalDataRoot.WellBoreW3C.Add(wellBoreW3C);

                                        break;
                                    }
                                    case DbfTape.WB14B2RM:
                                    {
                                        wellBore14B2Remarks = new WellBore14B2Remarks(template.Record);
                                        wellBore14B2Well.WellBore14B2Remarks.Add(wellBore14B2Remarks);

                                        break;
                                    }
                                    default: throw new ArgumentOutOfRangeException();
                                }

                                offset += 247;

                                if(counter % 100000 == 0)
                                {
                                    using(NpgsqlTransaction transaction = Connection.BeginTransaction(IsolationLevel.ReadCommitted))
                                    {
                                        WellBoreTechnicalDataRootTable.AddRange(wellBoreTechnicalDataRoots);

                                        transaction.Commit();

                                        SaveChanges();
                                    }

                                    wellBoreTechnicalDataRoots.Clear();

                                    Console.WriteLine($"{offset}:{counter}");
                                }
                            }
                        }
                    }
                }

                using(NpgsqlTransaction transaction = Connection.BeginTransaction(IsolationLevel.ReadCommitted))
                {
                    WellBoreTechnicalDataRootTable.AddRange(wellBoreTechnicalDataRoots);

                    transaction.Commit();

                    SaveChanges();
                }
            }
        }
    }
}
