using System;
using System.Collections.Generic;
using System.Data;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using System.Security;
using System.Text.RegularExpressions;

using Engineering.DataSource.Tools;

using Kokkos;

using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Infrastructure;
using Microsoft.EntityFrameworkCore.Storage;

using Npgsql;

namespace OilGas.Data.RRC.Texas
{
    /// <summary>psql --username= --password --host=timothyrmcnealy.com --port=5432 --dbname=TexasDumpWL</summary>
    public sealed class TexasDumpWLDbContext : DbContext
    {
        public const string Host = "timothyrmcnealy.com";
        public const string Port = "15432";

        //ON CONFLICT "Unique_Idx" DO NOTHING

        private static readonly Regex fixQuotesRegex1 = new Regex("'([a-zA-Z0-9]+)'",  RegexOptions.Multiline | RegexOptions.ExplicitCapture | RegexOptions.Compiled);
        private static readonly Regex fixQuotesRegex2 = new Regex("'([a-zA-Z0-9]+)\"", RegexOptions.Multiline | RegexOptions.ExplicitCapture | RegexOptions.Compiled);
        private static readonly Regex fixQuotesRegex3 = new Regex("\"([a-zA-Z0-9]+)'", RegexOptions.Multiline | RegexOptions.ExplicitCapture | RegexOptions.Compiled);

        private static readonly Regex fixLineEndingRegex = new Regex("\r\n\"\r\n", RegexOptions.Multiline | RegexOptions.ExplicitCapture | RegexOptions.Compiled);

        private static readonly Regex removeIdColumnRegex = new Regex("^([0-9]+),", RegexOptions.Multiline | RegexOptions.ExplicitCapture | RegexOptions.Compiled);

        public DbSet<GasAllowableTransfersSegment> GasAllowableTransfersSegmentTable { get; set; }

        public DbSet<GasFormG10PreviousSegment> GasFormG10PreviousSegmentTable { get; set; }

        public DbSet<GasFormG10Segment> GasFormG10SegmentTable { get; set; }

        public DbSet<GasFormG1Segment> GasFormG1SegmentTable { get; set; }

        public DbSet<GasFormGC1Segment> GasFormGC1SegmentTable { get; set; }

        public DbSet<GasFormsLackingDescriptionSegment> GasFormsLackingDescriptionSegmentTable { get; set; }

        public DbSet<GasFormsLackingSegment> GasFormsLackingSegmentTable { get; set; }

        public DbSet<GasPreviousAllowableSegment> GasPreviousAllowableSegmentTable { get; set; }

        public DbSet<GasReportingCycleSegment> GasReportingCycleSegmentTable { get; set; }

        public DbSet<GasScheduleUICSegment> GasScheduleUICSegmentTable { get; set; }

        public DbSet<GasSealedWellSegment> GasSealedWellSegmentTable { get; set; }

        public DbSet<GasSenateBill126Segment> GasSenateBill126SegmentTable { get; set; }

        public DbSet<GasWellRemarksSegment> GasWellRemarksSegmentTable { get; set; }

        public DbSet<GasWellRootSegment> GasWellRootSegmentTable { get; set; }

        public DbSet<OilAllowableTransfersSegment> OilAllowableTransfersSegmentTable { get; set; }

        public DbSet<OilDesignatedCasingLeakageWellSegment> OilDesignatedCasingLeakageWellSegmentTable { get; set; }

        public DbSet<OilEastTexasSegment> OilEastTexasSegmentTable { get; set; }

        public DbSet<OilFormsLackingDescriptionSegment> OilFormsLackingDescriptionSegmentTable { get; set; }

        public DbSet<OilFormsLackingSegment> OilFormsLackingSegmentTable { get; set; }

        public DbSet<OilFormW10PreviousSegment> OilFormW10PreviousSegmentTable { get; set; }

        public DbSet<OilFormW10Segment> OilFormW10SegmentTable { get; set; }

        public DbSet<OilPanhandleGasBalancingSegment> OilPanhandleGasBalancingSegmentTable { get; set; }

        public DbSet<OilPanhandleGasProductionSegment> OilPanhandleGasProductionSegmentTable { get; set; }

        public DbSet<OilPreviousAllowableSegment> OilPreviousAllowableSegmentTable { get; set; }

        public DbSet<OilPreviousOilWellTypesSegment> OilPreviousOilWellTypesSegmentTable { get; set; }

        public DbSet<OilReportingCycleSegment> OilReportingCycleSegmentTable { get; set; }

        public DbSet<OilScheduleUICSegment> OilScheduleUICSegmentTable { get; set; }

        public DbSet<OilSealedWellSegment> OilSealedWellSegmentTable { get; set; }

        public DbSet<OilSenateBill126Segment> OilSenateBill126SegmentTable { get; set; }

        public DbSet<OilTransferredWellsSegment> OilTransferredWellsSegmentTable { get; set; }

        public DbSet<OilWellRemark> OilWellRemarkTable { get; set; }

        public DbSet<OilWellRootSegment> OilWellRootSegmentTable { get; set; }


        public NpgsqlConnection Connection { get; }

        public TexasDumpWLDbContext()
            : this(CreateAndOpen())
        {
        }

        public TexasDumpWLDbContext(NpgsqlConnection connection)
            : base(new DbContextOptionsBuilder<TexasDumpWLDbContext>().UseNpgsql(connection).Options)
        {
            Connection = connection;

            Database.EnsureCreated();
        }

        private static NpgsqlConnection CreateAndOpen()
        {
            NpgsqlConnection connection;

            try
            {
                connection = new NpgsqlConnection($"Host={Host};Port={Port};Username={Encryption.Username};Password={Encryption.Password};Database=TexasDumpWL");

                connection.Open();
            }
            catch(Exception)
            {
                connection = new NpgsqlConnection($"Host={Host};Port={Port};Username=db_user;Password=dbAccess;Database=TexasDumpWL");

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


        public void LoadFieldInformation_olf(string filePath)
        {
            //List<OilLedger> oilLedgers = new List<OilLedger>(250000);

            //using(ScopeGuard.Get())
            //{
            //    using(MemoryMapped srcMM = new MemoryMapped(filePath))
            //    {
            //        unsafe
            //        {
            //            byte* ptr = srcMM.GetPointer<byte>();

            //            ulong offset = 0;

            //            ReadOnlySpan<byte> str;

            //            while(srcMM.Size() > offset)
            //            {
            //                str = new ReadOnlySpan<byte>(ptr + offset, 1200);

            //                oilLedgers.Add(new OilLedger(str));

            //                offset += 1200;
            //            }
            //        }
            //    }

            //    using(NpgsqlTransaction transaction = Connection.BeginTransaction(IsolationLevel.ReadCommitted))
            //    {
            //        OilLedgerTable.AddRange(oilLedgers);

            //        transaction.Commit();

            //        SaveChanges();
            //    }
            //}
        }

        public void LoadFieldInformation_gsf(string filePath)
        {
            //List<GasLedger> gasLedgers = new List<GasLedger>(250000);

            //using(ScopeGuard.Get())
            //{
            //    using(MemoryMapped srcMM = new MemoryMapped(filePath))
            //    {
            //        unsafe
            //        {
            //            byte* ptr = srcMM.GetPointer<byte>();

            //            ulong offset = 0;

            //            ReadOnlySpan<byte> str;

            //            while(srcMM.Size() > offset)
            //            {
            //                str = new ReadOnlySpan<byte>(ptr + offset, 1200);

            //                gasLedgers.Add(new GasLedger(str));

            //                offset += 1200;
            //            }
            //        }
            //    }

            //    using(NpgsqlTransaction transaction = Connection.BeginTransaction(IsolationLevel.ReadCommitted))
            //    {
            //        GasLedgerTable.AddRange(gasLedgers);

            //        transaction.Commit();

            //        SaveChanges();
            //    }
            //}
        }

        public void LoadFieldInformation_wlf100(params string[] filePaths)
        {
            using(ScopeGuard.Get())
            {
                OilWellRootSegment                    oilWellRootSegment                    = null;
                OilReportingCycleSegment              oilReportingCycleSegment              = null;
                OilPreviousAllowableSegment           oilPreviousAllowableSegment           = null;
                OilFormsLackingSegment                oilFormsLackingSegment                = null;
                OilFormsLackingDescriptionSegment     oilFormsLackingDescriptionSegment     = null;
                OilEastTexasSegment                   oilEastTexasSegment                   = null;
                OilDesignatedCasingLeakageWellSegment oilDesignatedCasingLeakageWellSegment = null;
                OilAllowableTransfersSegment          oilAllowableTransfersSegment          = null;
                OilTransferredWellsSegment            oilTransferredWellsSegment            = null;
                OilPanhandleGasProductionSegment      oilPanhandleGasProductionSegment      = null;
                OilPreviousOilWellTypesSegment        oilPreviousOilWellTypesSegment        = null;
                OilPanhandleGasBalancingSegment       oilPanhandleGasBalancingSegment       = null;
                OilFormW10Segment                     oilFormW10Segment                     = null;
                OilFormW10PreviousSegment             oilFormW10PreviousSegment             = null;
                OilWellRemark                         oilWellRemark                         = null;
                OilSealedWellSegment                  oilSealedWellSegment                  = null;
                OilScheduleUICSegment                 oilScheduleUicSegment                 = null;
                OilSenateBill126Segment               oilSenateBill126Segment               = null;

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
                                str = new ReadOnlySpan<byte>(ptr + offset, 228);

                                template = new RecordLayoutTemplate(str);

                                switch((OilTape)template.GetKeyValue())
                                {
                                    case OilTape.WLROOT:
                                    {
                                        if(oilWellRootSegment != null)
                                        {
                                            //if(oilReportingCycleSegment != null)
                                            //    Add(oilReportingCycleSegment);

                                            //if(oilPreviousAllowableSegment != null)
                                            //    Add(oilPreviousAllowableSegment);

                                            //if(oilFormsLackingSegment != null)
                                            //    Add(oilFormsLackingSegment);

                                            //if(oilFormsLackingDescriptionSegment != null)
                                            //    Add(oilFormsLackingDescriptionSegment);

                                            //if(oilEastTexasSegment != null)
                                            //    Add(oilEastTexasSegment);

                                            //if(oilDesignatedCasingLeakageWellSegment != null)
                                            //    Add(oilDesignatedCasingLeakageWellSegment);

                                            //if(oilAllowableTransfersSegment != null)
                                            //    Add(oilAllowableTransfersSegment);

                                            //if(oilTransferredWellsSegment != null)
                                            //    Add(oilTransferredWellsSegment);

                                            //if(oilPanhandleGasProductionSegment != null)
                                            //    Add(oilPanhandleGasProductionSegment);

                                            //if(oilPreviousOilWellTypesSegment != null)
                                            //    Add(oilPreviousOilWellTypesSegment);

                                            //if(oilPanhandleGasBalancingSegment != null)
                                            //    Add(oilPanhandleGasBalancingSegment);

                                            //if(oilFormW10Segment != null)
                                            //    Add(oilFormW10Segment);

                                            //if(oilFormW10PreviousSegment != null)
                                            //    Add(oilFormW10PreviousSegment);

                                            //if(oilWellRemark != null)
                                            //    Add(oilWellRemark);

                                            //if(oilSealedWellSegment != null)
                                            //    Add(oilSealedWellSegment);

                                            //if(oilScheduleUicSegment != null)
                                            //    Add(oilScheduleUicSegment);

                                            //if(oilSenateBill126Segment != null)
                                            //    Add(oilSenateBill126Segment);

                                            Add(oilWellRootSegment);

                                            oilWellRootSegment = null;
                                        }

                                        oilWellRootSegment = new OilWellRootSegment(template.Record);

                                        oilReportingCycleSegment              = null;
                                        oilPreviousAllowableSegment           = null;
                                        oilFormsLackingSegment                = null;
                                        oilFormsLackingDescriptionSegment     = null;
                                        oilEastTexasSegment                   = null;
                                        oilDesignatedCasingLeakageWellSegment = null;
                                        oilAllowableTransfersSegment          = null;
                                        oilTransferredWellsSegment            = null;
                                        oilPanhandleGasProductionSegment      = null;
                                        oilPreviousOilWellTypesSegment        = null;
                                        oilPanhandleGasBalancingSegment       = null;
                                        oilFormW10Segment                     = null;
                                        oilFormW10PreviousSegment             = null;
                                        oilWellRemark                         = null;
                                        oilSealedWellSegment                  = null;
                                        oilScheduleUicSegment                 = null;
                                        oilSenateBill126Segment               = null;

                                        break;
                                    }
                                    case OilTape.WLORPTCY:
                                    {
                                        oilReportingCycleSegment                    = new OilReportingCycleSegment(template.Record);
                                        oilWellRootSegment.OilReportingCycleSegment = oilReportingCycleSegment;

                                        break;
                                    }
                                    case OilTape.WLOPRVAL:
                                    {
                                        oilPreviousAllowableSegment                          = new OilPreviousAllowableSegment(template.Record);
                                        oilReportingCycleSegment.OilPreviousAllowableSegment = oilPreviousAllowableSegment;

                                        break;
                                    }
                                    case OilTape.WLOFRMLK:
                                    {
                                        oilFormsLackingSegment                          = new OilFormsLackingSegment(template.Record);
                                        oilReportingCycleSegment.OilFormsLackingSegment = oilFormsLackingSegment;

                                        break;
                                    }
                                    case OilTape.WLOFRMDS:
                                    {
                                        oilFormsLackingDescriptionSegment                        = new OilFormsLackingDescriptionSegment(template.Record);
                                        oilFormsLackingSegment.OilFormsLackingDescriptionSegment = oilFormsLackingDescriptionSegment;

                                        break;
                                    }
                                    case OilTape.WLOETEX:
                                    {
                                        oilEastTexasSegment                          = new OilEastTexasSegment(template.Record);
                                        oilReportingCycleSegment.OilEastTexasSegment = oilEastTexasSegment;

                                        break;
                                    }
                                    case OilTape.WLOETDS:
                                    {
                                        oilDesignatedCasingLeakageWellSegment                     = new OilDesignatedCasingLeakageWellSegment(template.Record);
                                        oilEastTexasSegment.OilDesignatedCasingLeakageWellSegment = oilDesignatedCasingLeakageWellSegment;

                                        break;
                                    }
                                    case OilTape.WLOALTF:
                                    {
                                        oilAllowableTransfersSegment                          = new OilAllowableTransfersSegment(template.Record);
                                        oilReportingCycleSegment.OilAllowableTransfersSegment = oilAllowableTransfersSegment;

                                        break;
                                    }
                                    case OilTape.WLOTFWL:
                                    {
                                        oilTransferredWellsSegment                              = new OilTransferredWellsSegment(template.Record);
                                        oilAllowableTransfersSegment.OilTransferredWellsSegment = oilTransferredWellsSegment;

                                        break;
                                    }
                                    case OilTape.WLPHPROD:
                                    {
                                        oilPanhandleGasProductionSegment                          = new OilPanhandleGasProductionSegment(template.Record);
                                        oilReportingCycleSegment.OilPanhandleGasProductionSegment = oilPanhandleGasProductionSegment;

                                        break;
                                    }
                                    case OilTape.WLPVTYP:
                                    {
                                        oilPreviousOilWellTypesSegment                    = new OilPreviousOilWellTypesSegment(template.Record);
                                        oilWellRootSegment.OilPreviousOilWellTypesSegment = oilPreviousOilWellTypesSegment;

                                        break;
                                    }
                                    case OilTape.WLPHBAL:
                                    {
                                        oilPanhandleGasBalancingSegment                    = new OilPanhandleGasBalancingSegment(template.Record);
                                        oilWellRootSegment.OilPanhandleGasBalancingSegment = oilPanhandleGasBalancingSegment;

                                        break;
                                    }
                                    case OilTape.WLOW10:
                                    {
                                        oilFormW10Segment                    = new OilFormW10Segment(template.Record);
                                        oilWellRootSegment.OilFormW10Segment = oilFormW10Segment;

                                        break;
                                    }
                                    case OilTape.WLOPVW10:
                                    {
                                        oilFormW10PreviousSegment                   = new OilFormW10PreviousSegment(template.Record);
                                        oilFormW10Segment.OilFormW10PreviousSegment = oilFormW10PreviousSegment;

                                        break;
                                    }
                                    case OilTape.WLREMARK:
                                    {
                                        oilWellRemark                    = new OilWellRemark(template.Record);
                                        oilWellRootSegment.OilWellRemark = oilWellRemark;

                                        break;
                                    }
                                    case OilTape.WLSEAL:
                                    {
                                        oilSealedWellSegment                    = new OilSealedWellSegment(template.Record);
                                        oilWellRootSegment.OilSealedWellSegment = oilSealedWellSegment;

                                        break;
                                    }
                                    case OilTape.WLOUIC:
                                    {
                                        oilScheduleUicSegment                          = new OilScheduleUICSegment(template.Record);
                                        oilReportingCycleSegment.OilScheduleUICSegment = oilScheduleUicSegment;

                                        break;
                                    }
                                    case OilTape.WLSB126:
                                    {
                                        oilSenateBill126Segment                    = new OilSenateBill126Segment(template.Record);
                                        oilWellRootSegment.OilSenateBill126Segment = oilSenateBill126Segment;

                                        break;
                                    }
                                    default: throw new ArgumentOutOfRangeException();
                                }

                                offset += 228;
                            }
                        }
                    }
                }

                using(NpgsqlTransaction transaction = Connection.BeginTransaction(IsolationLevel.ReadCommitted))
                {
                    transaction.Commit();

                    SaveChanges();
                }
            }
        }

        public void LoadFieldInformation_wlf101(params string[] filePaths)
        {
            using(ScopeGuard.Get())
            {
                GasWellRootSegment                gasWellRootSegment                = null;
                GasWellRemarksSegment             gasWellRemarksSegment             = null;
                GasSealedWellSegment              gasSealedWellSegment              = null;
                GasFormG1Segment                  gasFormG1Segment                  = null;
                GasFormG10Segment                 gasFormG10Segment                 = null;
                GasFormG10PreviousSegment         gasFormG10PreviousSegment         = null;
                GasFormGC1Segment                 gasFormGC1Segment                 = null;
                GasReportingCycleSegment          gasReportingCycleSegment          = null;
                GasFormsLackingSegment            gasFormsLackingSegment            = null;
                GasFormsLackingDescriptionSegment gasFormsLackingDescriptionSegment = null;
                GasAllowableTransfersSegment      gasAllowableTransfersSegment      = null;
                GasPreviousAllowableSegment       gasPreviousAllowableSegment       = null;
                GasScheduleUICSegment             gasScheduleUICSegment             = null;
                GasSenateBill126Segment           gasSenateBill126Segment           = null;

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
                                str = new ReadOnlySpan<byte>(ptr + offset, 228);

                                template = new RecordLayoutTemplate(str);

                                switch((GasTape)template.GetKeyValue())
                                {
                                    case GasTape.WLROOT:
                                    {
                                        if(gasWellRootSegment != null)
                                        {
                                            //if(gasWellRemarksSegment != null)
                                            //    Add(gasWellRemarksSegment);

                                            //if(gasSealedWellSegment != null)
                                            //    Add(gasSealedWellSegment);

                                            //if(gasFormG1Segment != null)
                                            //    Add(gasFormG1Segment);

                                            //if(gasFormG10Segment != null)
                                            //    Add(gasFormG10Segment);

                                            //if(gasFormG10PreviousSegment != null)
                                            //    Add(gasFormG10PreviousSegment);

                                            //if(gasFormGC1Segment != null)
                                            //    Add(gasFormGC1Segment);

                                            //if(gasReportingCycleSegment != null)
                                            //    Add(gasReportingCycleSegment);

                                            //if(gasFormsLackingSegment != null)
                                            //    Add(gasFormsLackingSegment);

                                            //if(gasFormsLackingDescriptionSegment != null)
                                            //    Add(gasFormsLackingDescriptionSegment);

                                            //if(gasAllowableTransfersSegment != null)
                                            //    Add(gasAllowableTransfersSegment);

                                            //if(gasPreviousAllowableSegment != null)
                                            //    Add(gasPreviousAllowableSegment);

                                            //if(gasScheduleUICSegment != null)
                                            //    Add(gasScheduleUICSegment);

                                            //if(gasSenateBill126Segment != null)
                                            //    Add(gasSenateBill126Segment);

                                            Add(gasWellRootSegment);

                                            gasWellRootSegment = null;
                                        }

                                        gasWellRootSegment = new GasWellRootSegment(template.Record);

                                        gasWellRemarksSegment             = null;
                                        gasSealedWellSegment              = null;
                                        gasFormG1Segment                  = null;
                                        gasFormG10Segment                 = null;
                                        gasFormG10PreviousSegment         = null;
                                        gasFormGC1Segment                 = null;
                                        gasReportingCycleSegment          = null;
                                        gasFormsLackingSegment            = null;
                                        gasFormsLackingDescriptionSegment = null;
                                        gasAllowableTransfersSegment      = null;
                                        gasPreviousAllowableSegment       = null;
                                        gasScheduleUICSegment             = null;
                                        gasSenateBill126Segment           = null;

                                        break;
                                    }
                                    case GasTape.WLREMARK:
                                    {
                                        gasWellRemarksSegment                    = new GasWellRemarksSegment(template.Record);
                                        gasWellRootSegment.GasWellRemarksSegment = gasWellRemarksSegment;

                                        break;
                                    }
                                    case GasTape.WLSEAL:
                                    {
                                        gasSealedWellSegment                    = new GasSealedWellSegment(template.Record);
                                        gasWellRootSegment.GasSealedWellSegment = gasSealedWellSegment;

                                        break;
                                    }
                                    case GasTape.WLGG1:
                                    {
                                        gasFormG1Segment                    = new GasFormG1Segment(template.Record);
                                        gasWellRootSegment.GasFormG1Segment = gasFormG1Segment;

                                        break;
                                    }
                                    case GasTape.WLGG10:
                                    {
                                        gasFormG10Segment                    = new GasFormG10Segment(template.Record);
                                        gasWellRootSegment.GasFormG10Segment = gasFormG10Segment;

                                        break;
                                    }
                                    case GasTape.WLGPVG10:
                                    {
                                        gasFormG10PreviousSegment                   = new GasFormG10PreviousSegment(template.Record);
                                        gasFormG10Segment.GasFormG10PreviousSegment = gasFormG10PreviousSegment;

                                        break;
                                    }
                                    case GasTape.WLGC1:
                                    {
                                        gasFormGC1Segment                    = new GasFormGC1Segment(template.Record);
                                        gasWellRootSegment.GasFormGC1Segment = gasFormGC1Segment;

                                        break;
                                    }
                                    case GasTape.WLGRPTCY:
                                    {
                                        gasReportingCycleSegment                    = new GasReportingCycleSegment(template.Record);
                                        gasWellRootSegment.GasReportingCycleSegment = gasReportingCycleSegment;

                                        break;
                                    }
                                    case GasTape.WLGFRMLK:
                                    {
                                        gasFormsLackingSegment                          = new GasFormsLackingSegment(template.Record);
                                        gasReportingCycleSegment.GasFormsLackingSegment = gasFormsLackingSegment;

                                        break;
                                    }
                                    case GasTape.WLGFRMDS:
                                    {
                                        gasFormsLackingDescriptionSegment                        = new GasFormsLackingDescriptionSegment(template.Record);
                                        gasFormsLackingSegment.GasFormsLackingDescriptionSegment = gasFormsLackingDescriptionSegment;

                                        break;
                                    }
                                    case GasTape.WLGALTF:
                                    {
                                        gasAllowableTransfersSegment                          = new GasAllowableTransfersSegment(template.Record);
                                        gasReportingCycleSegment.GasAllowableTransfersSegment = gasAllowableTransfersSegment;

                                        break;
                                    }
                                    case GasTape.WLGPRVAL:
                                    {
                                        gasPreviousAllowableSegment                          = new GasPreviousAllowableSegment(template.Record);
                                        gasReportingCycleSegment.GasPreviousAllowableSegment = gasPreviousAllowableSegment;

                                        break;
                                    }
                                    case GasTape.WLGUIC:
                                    {
                                        gasScheduleUICSegment                          = new GasScheduleUICSegment(template.Record);
                                        gasReportingCycleSegment.GasScheduleUICSegment = gasScheduleUICSegment;

                                        break;
                                    }
                                    case GasTape.WLSB126:
                                    {
                                        gasSenateBill126Segment                    = new GasSenateBill126Segment(template.Record);
                                        gasWellRootSegment.GasSenateBill126Segment = gasSenateBill126Segment;

                                        break;
                                    }
                                    default: throw new ArgumentOutOfRangeException();
                                }

                                offset += 228;
                            }
                        }
                    }
                }

                //using(NpgsqlTransaction transaction = Connection.BeginTransaction(IsolationLevel.ReadCommitted))
                //{
                //    transaction.Commit();

                //    SaveChanges();
                //}
            }
        }
    }
}
