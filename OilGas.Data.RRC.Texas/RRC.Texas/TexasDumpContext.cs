﻿using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Data;
using System.IO;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Security;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

using Engineering.DataSource.Tools;

using Kokkos;

using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Infrastructure;
using Microsoft.EntityFrameworkCore.Storage;
using Microsoft.Extensions.Primitives;

using Npgsql;

namespace OilGas.Data.RRC.Texas
{
    /// <summary>psql --username=trmcnealy --password --host=timothyrmcnealy.com --port=5432 --dbname=TexasDump</summary>
    public sealed class TexasDumpDbContext : DbContext
    {
        //ON CONFLICT "Unique_Idx" DO NOTHING

        private static readonly Regex fixQuotesRegex1 = new Regex("'([a-zA-Z0-9]+)'",  RegexOptions.Multiline | RegexOptions.ExplicitCapture | RegexOptions.Compiled);
        private static readonly Regex fixQuotesRegex2 = new Regex("'([a-zA-Z0-9]+)\"", RegexOptions.Multiline | RegexOptions.ExplicitCapture | RegexOptions.Compiled);
        private static readonly Regex fixQuotesRegex3 = new Regex("\"([a-zA-Z0-9]+)'", RegexOptions.Multiline | RegexOptions.ExplicitCapture | RegexOptions.Compiled);

        private static readonly Regex fixLineEndingRegex = new Regex("\r\n\"\r\n", RegexOptions.Multiline | RegexOptions.ExplicitCapture | RegexOptions.Compiled);

        private static readonly Regex removeIdColumnRegex = new Regex("^([0-9]+),", RegexOptions.Multiline | RegexOptions.ExplicitCapture | RegexOptions.Compiled);

        public DbSet<OgFieldCycle> OgFieldCycleTable { get; set; }

        /// <summary>
        ///     CREATE TABLE "OgLeaseCycleData" ( "Id" int4 NOT NULL GENERATED BY DEFAULT AS IDENTITY, "OIL_GAS_CODE"
        ///     bpchar(1) NOT NULL, "DISTRICT_NO" int4 NOT NULL, "LEASE_NO" int8 NOT NULL, "CYCLE_YEAR" int4 NOT NULL,
        ///     "CYCLE_MONTH" int4 NOT NULL, "CYCLE_YEAR_MONTH" int4 NOT NULL, "LEASE_NO_DISTRICT_NO" int8 NOT NULL, "OPERATOR_NO"
        ///     int8 NULL, "FIELD_NO" int8 NULL, "FIELD_TYPE" text NULL, "GAS_WELL_NO" text NULL, "PROD_REPORT_FILED_FLAG"
        ///     bpchar(1) NULL, "LEASE_OIL_PROD_VOL" float4 NULL, "LEASE_OIL_ALLOW" float4 NULL, "LEASE_OIL_ENDING_BAL" float4
        ///     NULL, "LEASE_GAS_PROD_VOL" float4 NULL, "LEASE_GAS_ALLOW" float4 NULL, "LEASE_GAS_LIFT_INJ_VOL" float4 NULL,
        ///     "LEASE_COND_PROD_VOL" float4 NULL, "LEASE_COND_LIMIT" float4 NULL, "LEASE_COND_ENDING_BAL" float4 NULL,
        ///     "LEASE_CSGD_PROD_VOL" float4 NULL, "LEASE_CSGD_LIMIT" float4 NULL, "LEASE_CSGD_GAS_LIFT" float4 NULL,
        ///     "LEASE_OIL_TOT_DISP" float4 NULL, "LEASE_GAS_TOT_DISP" float4 NULL, "LEASE_COND_TOT_DISP" float4 NULL,
        ///     "LEASE_CSGD_TOT_DISP" float4 NULL, "DISTRICT_NAME" text NULL, "LEASE_NAME" text NULL, "OPERATOR_NAME" text NULL,
        ///     "FIELD_NAME" text NULL ) PARTITION BY RANGE ("Id"); ALTER TABLE "OgLeaseCycleData" ADD CONSTRAINT
        ///     "PK_OgLeaseCycleData" PRIMARY KEY ("Id")
        /// </summary>
        public DbSet<OgLeaseCycleData> OgLeaseCycleDataTable { get; set; }

        public DbSet<OgWellCompletionData> OgWellCompletionDataTable { get; set; }

        public DbSet<OgFieldDw> OgFieldDwTable { get; set; }

        public DbSet<OgOperatorDw> OgOperatorDwTable { get; set; }

        public DbSet<OgRegulatoryLeaseDw> OgRegulatoryLeaseDwTable { get; set; }

        public DbSet<FieldInformation> FieldInformationTable { get; set; }

        public DbSet<FieldGasInformation> FieldGasInformationTable { get; set; }

        public DbSet<Field49BCalculations> Field49BCalculationsTable { get; set; }

        public DbSet<WellS> WellSTable { get; set; }

        public DbSet<WellB> WellBTable { get; set; }

        public NpgsqlConnection Connection { get; }

        public TexasDumpDbContext()
            : this(CreateAndOpen())
        {
        }

        public TexasDumpDbContext(NpgsqlConnection connection)
            : base(new DbContextOptionsBuilder<TexasDumpDbContext>().UseNpgsql(connection).Options)
        {
            Connection = connection;

            Database.EnsureCreated();
        }

        private static NpgsqlConnection CreateAndOpen()
        {
            NpgsqlConnection connection;

            try
            {
                connection = new NpgsqlConnection($"Host=timothyrmcnealy.com;Port=5432;Username={Encryption.Username};Password={Encryption.Password};Database=TexasDump");

                connection.Open();
            }
            catch(Exception)
            {
                connection = new NpgsqlConnection("Host=timothyrmcnealy.com;Port=5432;Username=db_user;Password=dbAccess;Database=TexasDump");

                connection.Open();
            }

            return connection;
        }

        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            base.OnModelCreating(modelBuilder);

            modelBuilder.Entity<OgWellCompletionData>()
                        .HasIndex(dp => new {dp.OIL_GAS_CODE, dp.DISTRICT_NO, dp.LEASE_NO, dp.WELL_NO, dp.API_COUNTY_CODE, dp.API_UNIQUE_NO})
                        .IsCreatedConcurrently()
                        .IsUnique();

            modelBuilder.Entity<OgLeaseCycleData>()
                        .HasIndex(dp => new {dp.OIL_GAS_CODE, dp.DISTRICT_NO, dp.LEASE_NO, dp.CYCLE_YEAR_MONTH, dp.OPERATOR_NO})
                        .IsCreatedConcurrently()
                        .IsUnique();

            modelBuilder.Entity<OgFieldCycle>()
                        .HasIndex(dp => new
                         {
                             dp.DISTRICT_NO,
                             dp.FIELD_NO,
                             dp.CYCLE_YEAR,
                             dp.CYCLE_MONTH,
                             dp.FIELD_OIL_PROD_VOL,
                             dp.FIELD_GAS_PROD_VOL,
                             dp.FIELD_COND_PROD_VOL,
                             dp.FIELD_CSGD_PROD_VOL
                         })
                        .IsCreatedConcurrently()
                        .IsUnique();

            modelBuilder.Entity<OgFieldDw>().HasIndex(dp => new {dp.FIELD_NO, dp.FIELD_NAME, dp.DISTRICT_NO}).IsCreatedConcurrently().IsUnique();
            modelBuilder.Entity<OgOperatorDw>().HasIndex(dp => new {dp.OPERATOR_NO}).IsCreatedConcurrently().IsUnique();

            modelBuilder.Entity<OgRegulatoryLeaseDw>()
                        .HasIndex(dp => new {dp.OIL_GAS_CODE, dp.DISTRICT_NO, dp.LEASE_NO, dp.OPERATOR_NO, dp.FIELD_NO, dp.WELL_NO})
                        .IsCreatedConcurrently()
                        .IsUnique();

            modelBuilder.Entity<FieldInformation>();
            modelBuilder.Entity<FieldGasInformation>();
            modelBuilder.Entity<Field49BCalculations>();
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

        public void LoadWellDbf()
        {
            int num_threads = 4;
            int num_numa    = 1;
            int device_id   = 0;
            int ndevices    = 1;
            int skip_device = 9999;

            InitArguments arguments = new InitArguments(num_threads, num_numa, device_id, ndevices, skip_device, false);

            List<string[]> s_rows, b_rows;

            using(ScopeGuard.Get(arguments))
            {
                //using (MemoryMapped l_mm = new MemoryMapped("R:/dbase/l.dbf.csv"))
                using(MemoryMapped s_mm = new MemoryMapped("R:/s.dbf.csv"))
                using(MemoryMapped b_mm = new MemoryMapped("R:/b.dbf.csv"))
                {
                    //MappedCsvReader csvReader = new MappedCsvReader(l_mm);
                    //(_, l_rows) = csvReader.ReadFile(1);

                    MappedCsvReader csvReader = new MappedCsvReader(s_mm);
                    (_, s_rows) = csvReader.ReadFile(1);

                    csvReader   = new MappedCsvReader(b_mm);
                    (_, b_rows) = csvReader.ReadFile(1);

                    List<WellS> wellSs = new List<WellS>(s_rows.Count);

                    Parallel.ForEach(Partitioner.Create(0, s_rows.Count),
                                     row =>
                                     {
                                         for(int i = row.Item1; i < row.Item2; i++)
                                         {
                                             wellSs.Add(new WellS(s_rows[i]));
                                         }
                                     });

                    using(NpgsqlTransaction transaction = Connection.BeginTransaction(IsolationLevel.ReadCommitted))
                    {
                        WellSTable.AddRange(wellSs);

                        transaction.Commit();

                        SaveChanges();
                    }

                    //

                    List<WellB> wellBs = new List<WellB>(b_rows.Count);

                    Parallel.ForEach(Partitioner.Create(0, b_rows.Count),
                                     row =>
                                     {
                                         for(int i = row.Item1; i < row.Item2; i++)
                                         {
                                             wellBs.Add(new WellB(b_rows[i]));
                                         }
                                     });

                    using(NpgsqlTransaction transaction = Connection.BeginTransaction(IsolationLevel.ReadCommitted))
                    {
                        WellBTable.AddRange(wellBs);

                        transaction.Commit();

                        SaveChanges();
                    }
                }
            }
        }

        public void LoadOgFieldDwDsv(string filePath)
        {
            int num_threads = 4;
            int num_numa    = 1;
            int device_id   = 0;
            int ndevices    = 1;
            int skip_device = 9999;

            InitArguments arguments = new InitArguments(num_threads, num_numa, device_id, ndevices, skip_device, false);

            List<StringSegment[]> rows;

            using(ScopeGuard.Get(arguments))
            {
                using(MemoryMapped mm = new MemoryMapped(filePath))
                {
                    MappedDsvReader csvReader = new MappedDsvReader(mm);

                    (_, rows) = csvReader.ReadFile(1);
                }
            }

            OgFieldDw[] entries = new OgFieldDw[rows.Count];

            Parallel.ForEach(Partitioner.Create(0, rows.Count),
                             row =>
                             {
                                 for(int i = row.Item1; i < row.Item2; i++)
                                 {
                                     entries[i] = new OgFieldDw(rows[i]);
                                 }
                             });

            using(NpgsqlTransaction transaction = Connection.BeginTransaction(IsolationLevel.ReadCommitted))
            {
                OgFieldDwTable.AddRange(entries);

                transaction.Commit();

                SaveChanges();
            }
        }

        public void LoadOgRegulatoryLeaseDwDsv(string filePath)
        {
            int num_threads = 4;
            int num_numa    = 1;
            int device_id   = 0;
            int ndevices    = 1;
            int skip_device = 9999;

            InitArguments arguments = new InitArguments(num_threads, num_numa, device_id, ndevices, skip_device, false);

            List<StringSegment[]> rows;

            using(ScopeGuard.Get(arguments))
            {
                using(MemoryMapped mm = new MemoryMapped(filePath))
                {
                    MappedDsvReader csvReader = new MappedDsvReader(mm);

                    (_, rows) = csvReader.ReadFile(1);
                }
            }

            OgRegulatoryLeaseDw[] entries = new OgRegulatoryLeaseDw[rows.Count];

            Parallel.ForEach(Partitioner.Create(0, rows.Count),
                             row =>
                             {
                                 for(int i = row.Item1; i < row.Item2; i++)
                                 {
                                     entries[i] = new OgRegulatoryLeaseDw(rows[i]);
                                 }
                             });

            using(NpgsqlTransaction transaction = Connection.BeginTransaction(IsolationLevel.ReadCommitted))
            {
                OgRegulatoryLeaseDwTable.AddRange(entries);

                transaction.Commit();

                SaveChanges();
            }
        }

        public void LoadOgOperatorDwDsv(string filePath)
        {
            int num_threads = 4;
            int num_numa    = 1;
            int device_id   = 0;
            int ndevices    = 1;
            int skip_device = 9999;

            InitArguments arguments = new InitArguments(num_threads, num_numa, device_id, ndevices, skip_device, false);

            List<StringSegment[]> rows;

            using(ScopeGuard.Get(arguments))
            {
                using(MemoryMapped mm = new MemoryMapped(filePath))
                {
                    MappedDsvReader csvReader = new MappedDsvReader(mm);

                    (_, rows) = csvReader.ReadFile(1);
                }
            }

            OgOperatorDw[] entries = new OgOperatorDw[rows.Count];

            Parallel.ForEach(Partitioner.Create(0, rows.Count),
                             row =>
                             {
                                 for(int i = row.Item1; i < row.Item2; i++)
                                 {
                                     entries[i] = new OgOperatorDw(rows[i]);
                                 }
                             });

            using(NpgsqlTransaction transaction = Connection.BeginTransaction(IsolationLevel.ReadCommitted))
            {
                OgOperatorDwTable.AddRange(entries);

                transaction.Commit();

                SaveChanges();
            }
        }

        public void LoadOgFieldCycleDsv(string filePath)
        {
            int num_threads = 4;
            int num_numa    = 1;
            int device_id   = 0;
            int ndevices    = 1;
            int skip_device = 9999;

            InitArguments arguments = new InitArguments(num_threads, num_numa, device_id, ndevices, skip_device, false);

            List<StringSegment[]> rows;

            using(ScopeGuard.Get(arguments))
            {
                using(MemoryMapped mm = new MemoryMapped(filePath))
                {
                    MappedDsvReader csvReader = new MappedDsvReader(mm);

                    (_, rows) = csvReader.ReadFile(1);
                }
            }

            OgFieldCycle[] entries = new OgFieldCycle[rows.Count];

            //int lastUpdate = 2670000;

            //for(int i = 2670000; i < rows.Count; ++i)
            //{
            //    entries[i] = new OgFieldCycle(rows[i]);

            //    if(i % 100000 == 0)
            //    {
            //        using(NpgsqlTransaction transaction = Connection.BeginTransaction(IsolationLevel.ReadCommitted))
            //        {
            //            OgFieldCycleTable.AddRange(entries[lastUpdate..i]);
            //            lastUpdate = i;

            //            transaction.Commit();

            //            SaveChanges();
            //        }
            //    }
            //}

            Parallel.ForEach(Partitioner.Create(2670000, rows.Count),
                             row =>
                             {
                                 for(int i = row.Item1; i < row.Item2; i++)
                                 {
                                     entries[i] = new OgFieldCycle(rows[i]);
                                 }
                             });

            using(NpgsqlTransaction transaction = Connection.BeginTransaction(IsolationLevel.ReadCommitted))
            {
                OgFieldCycleTable.AddRange(entries);

                transaction.Commit();

                SaveChanges();
            }
        }

        //public void LoadOgLeaseCycleDataToCsv(string filePath)
        //{
        //    int num_threads = 4;
        //    int num_numa    = 1;
        //    int device_id   = 0;
        //    int ndevices    = 1;
        //    int skip_device = 9999;

        //    InitArguments arguments = new InitArguments(num_threads, num_numa, device_id, ndevices, skip_device, false);

        //    List<StringSegment[]> rows;

        //    using(ScopeGuard.Get(arguments))
        //    {
        //        using(MemoryMapped mm = new MemoryMapped(filePath))
        //        {
        //            unsafe
        //            {
        //                byte* data = mm.GetPointer<byte>();

        //                data.Re
        //            }
        //        }
        //    }
        //}

        public void LoadOgLeaseCycleDataDsv(string filePath,
                                            int    headerLineCount = 1)
        {
            int num_threads = 4;
            int num_numa    = 1;
            int device_id   = 0;
            int ndevices    = 1;
            int skip_device = 9999;

            InitArguments arguments = new InitArguments(num_threads, num_numa, device_id, ndevices, skip_device, false);

            List<StringSegment[]> rows;

            using(ScopeGuard.Get(arguments))
            {
                using(MemoryMapped mm = new MemoryMapped(filePath))
                {
                    MappedDsvReader csvReader = new MappedDsvReader(mm);

                    (_, rows) = csvReader.ReadFile(headerLineCount);
                }
            }

            //int lastUpdate = 2670000;

            //for(int i = 2670000; i < rows.Count; ++i)
            //{
            //    entries[i] = new OgFieldCycle(rows[i]);

            //    if(i % 100000 == 0)
            //    {
            //        using(NpgsqlTransaction transaction = Connection.BeginTransaction(IsolationLevel.ReadCommitted))
            //        {
            //            OgFieldCycleTable.AddRange(entries[lastUpdate..i]);
            //            lastUpdate = i;

            //            transaction.Commit();

            //            SaveChanges();
            //        }
            //    }
            //}

            //int parts    = 100;
            int partSize = rows.Count; // / parts;

            OgLeaseCycleData[] entries = BuildRows(rows, 0, partSize);

            Console.WriteLine("Rows built...");

            SaveRows(entries);

            //for(int p = 0; p < parts; ++p)
            //{
            //    entries = BuildRows(rows, p, partSize);

            //    try
            //    {
            //        SaveRows(entries).Wait();
            //    }
            //    catch(Exception)
            //    {
            //        //
            //    }
            //}

            //tdc.ReIndex("OgLeaseCycleData");
            //tdc.VacuumAndAnalyze("OgLeaseCycleData");
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

        public static void FixQuotes(string filePath)
        {
            string allText = File.ReadAllText(filePath);

            allText = fixQuotesRegex1.Replace(allText, "\"$1\"");

            allText = fixQuotesRegex2.Replace(allText, "\"$1\"");

            allText = fixQuotesRegex3.Replace(allText, "\"$1\"");

            allText = allText.Replace("\"\"", "\"");

            File.WriteAllText(filePath, allText);
        }

        public static void FixLineEnding(string filePath)
        {
            string allText = File.ReadAllText(filePath);

            allText = fixLineEndingRegex.Replace(allText, "\"\r\n");

            File.WriteAllText(filePath, allText);
        }

        public static void RemoveIdColumn(string filePath)
        {
            string allText = File.ReadAllText(filePath);

            allText = removeIdColumnRegex.Replace(allText, "");

            File.WriteAllText(filePath, allText);
        }

        public bool CopyFileToDb(string tableName,
                                 string columnNames,
                                 string filePath,
                                 string delimiter,
                                 bool   fixQuotes = true)
        {
            if(fixQuotes)
            {
                FixQuotes(filePath);
            }

            bool result = true;

            try
            {
                NpgsqlTransaction transaction = Connection.BeginTransaction();

                if(File.Exists(filePath))
                {
                    try
                    {
                        NpgsqlCommand command = Connection.CreateCommand();

                        command.CommandTimeout = 5 * 60;

                        command.CommandText = @$"CREATE TEMPORARY TABLE tmp_table AS SELECT * FROM ""{
                                tableName
                            }""; \COPY ""{
                                tableName
                            }"" ( {
                                columnNames
                            } ) FROM '{
                                filePath
                            }' DELIMITER '{
                                delimiter
                            }' CSV; INSERT INTO ""{
                                tableName
                            }"" SELECT * FROM tmp_table ON CONFLICT DO NOTHING; DROP TABLE tmp_table;";

                        command.ExecuteNonQuery();
                    }
                    catch(Exception e)
                    {
                        result = false;
                        transaction.Rollback();

                        throw e;
                    }
                    finally
                    {
                        if(result)
                        {
                            transaction.Commit();
                        }

                        transaction.Dispose();
                    }
                }
            }
            catch(Exception)
            {
                result = false;
            }

            return result;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public void SaveRows(OgLeaseCycleData[] entries)
        {
            int parts    = 50;
            int partSize = entries.Length / parts;

            List<List<OgLeaseCycleData>> segments = Partition(entries, partSize).ToList();

            for(int p = 0; p < parts; ++p)
            {
                Console.WriteLine($"segment {p}");

                try
                {
                    using NpgsqlTransaction transaction = Connection.BeginTransaction(IsolationLevel.ReadCommitted);

                    OgLeaseCycleDataTable.AddRange(segments[p]);

                    Console.Write("AddRangeAsync...");

                    transaction.Commit();

                    Console.Write("CommitAsync...");

                    SaveChanges();

                    Console.Write("SaveChangesAsync...");
                }
                catch(Exception)
                {
                    //Console.WriteLine(ex.Message);
                }
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        internal static OgLeaseCycleData[] BuildRows(List<StringSegment[]> rows,
                                                     int                   part,
                                                     int                   partSize)
        {
            OgLeaseCycleData[] entries = new OgLeaseCycleData[partSize];

            Parallel.ForEach(Partitioner.Create(0, partSize),
                             row =>
                             {
                                 //await using TexasDumpDbContext tdc = new TexasDumpDbContext();

                                 //OgLeaseCycleData data;

                                 for(int i = row.Item1; i < row.Item2; i++)
                                 {
                                     entries[i] = new OgLeaseCycleData(rows[i]);
                                     //data = new OgLeaseCycleData(rows[i + part * partSize]);

                                     //if(await GetLeaseCycleDataAsync(tdc, data))
                                     //{
                                     //entries[i] = data;
                                     //}
                                 }
                             });

            return entries;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static async Task<bool> GetLeaseCycleDataAsync(TexasDumpDbContext tdc,
                                                              OgLeaseCycleData   data)
        {
            OgLeaseCycleData ogLeaseCycleData = await tdc.OgLeaseCycleDataTable.AsNoTracking()
                                                         .FirstOrDefaultAsync(p => p.OIL_GAS_CODE     == data.OIL_GAS_CODE     &&
                                                                                   p.DISTRICT_NO      == data.DISTRICT_NO      &&
                                                                                   p.LEASE_NO         == data.LEASE_NO         &&
                                                                                   p.CYCLE_YEAR_MONTH == data.CYCLE_YEAR_MONTH &&
                                                                                   p.OPERATOR_NO      == data.OPERATOR_NO);

            return ogLeaseCycleData is null;
        }

        public void LoadOgWellCompletionDataDsv(string filePath)
        {
            int num_threads = 4;
            int num_numa    = 1;
            int device_id   = 0;
            int ndevices    = 1;
            int skip_device = 9999;

            InitArguments arguments = new InitArguments(num_threads, num_numa, device_id, ndevices, skip_device, false);

            List<StringSegment[]> rows;

            using(ScopeGuard.Get(arguments))
            {
                using(MemoryMapped mm = new MemoryMapped(filePath))
                {
                    MappedDsvReader csvReader = new MappedDsvReader(mm);

                    (_, rows) = csvReader.ReadFile(1);
                }
            }

            OgWellCompletionData[] entries = new OgWellCompletionData[rows.Count];

            //int lastUpdate = 2670000;

            //for(int i = 2670000; i < rows.Count; ++i)
            //{
            //    entries[i] = new OgFieldCycle(rows[i]);

            //    if(i % 100000 == 0)
            //    {
            //        using(NpgsqlTransaction transaction = Connection.BeginTransaction(IsolationLevel.ReadCommitted))
            //        {
            //            OgFieldCycleTable.AddRange(entries[lastUpdate..i]);
            //            lastUpdate = i;

            //            transaction.Commit();

            //            SaveChanges();
            //        }
            //    }
            //}

            Parallel.ForEach(Partitioner.Create(0, rows.Count),
                             row =>
                             {
                                 for(int i = row.Item1; i < row.Item2; i++)
                                 {
                                     entries[i] = new OgWellCompletionData(rows[i]);
                                 }
                             });

            using(NpgsqlTransaction transaction = Connection.BeginTransaction(IsolationLevel.ReadCommitted))
            {
                OgWellCompletionDataTable.AddRange(entries);

                transaction.Commit();

                SaveChanges();
            }
        }

        public void LoadFieldInformation_flf900(string filePath)
        {
            using(ScopeGuard.Get())
            {
                using(NpgsqlTransaction transaction = Connection.BeginTransaction(IsolationLevel.ReadCommitted))
                using(MemoryMapped srcMM = new MemoryMapped(filePath))
                {
                    unsafe
                    {
                        byte* ptr = srcMM.GetPointer<byte>();

                        RecordLayoutTemplate template;

                        ulong offset = 0;

                        ReadOnlySpan<byte> str;

                        FieldInformation fi = null;

                        FieldInformation     fi_query = null;
                        FieldGasInformation  fgi;
                        FieldGasInformation  fgi_query;
                        Field49BCalculations fc;
                        Field49BCalculations fc_query;

                        while(srcMM.Size() > offset)
                        {
                            str = new ReadOnlySpan<byte>(ptr + offset, 240);

                            template = new RecordLayoutTemplate(str);

                            switch(template.GetKeyValue())
                            {
                                case 1:
                                {
                                    if(fi != null && fi_query != null && (fi.FieldGasInformation != null || fi.Field49BCalculations != null))
                                    {
                                        FieldInformationTable.Update(fi);
                                    }

                                    fi = new FieldInformation(template.Record);

                                    fi_query = FieldInformationTable.FirstOrDefault(f => Nullable.Equals(f.FL_DISTRICT,               fi.FL_DISTRICT)               &&
                                                                                         Nullable.Equals(f.FL_FIELD_NUMBER,           fi.FL_FIELD_NUMBER)           &&
                                                                                         Nullable.Equals(f.FL_RESERVOIR_NUMBER,       fi.FL_RESERVOIR_NUMBER)       &&
                                                                                         Nullable.Equals(f.FL_FIELD_CLASS,            fi.FL_FIELD_CLASS)            &&
                                                                                         Nullable.Equals(f.FL_OIL_DISC_WELL_GRAVITY,  fi.FL_OIL_DISC_WELL_GRAVITY)  &&
                                                                                         Nullable.Equals(f.FL_ASSOC_OIL_FIELD_NUMBER, fi.FL_ASSOC_OIL_FIELD_NUMBER) &&
                                                                                         Nullable.Equals(f.FL_DISCOV_PERMIT_NUM,      fi.FL_DISCOV_PERMIT_NUM));

                                    if(fi_query != null)
                                    {
                                        fi = fi_query;
                                    }
                                    else
                                    {
                                        Console.WriteLine("FieldInformationTable.Add(fi);");
                                    }

                                    break;
                                }
                                case 2:
                                {
                                    fgi = new FieldGasInformation(template.Record);

                                    fgi_query = FieldGasInformationTable.FirstOrDefault(f => f.FL_GAS_DISC_CENTURY     == fgi.FL_GAS_DISC_CENTURY     &&
                                                                                             f.FL_GAS_DISC_YEAR        == fgi.FL_GAS_DISC_YEAR        &&
                                                                                             f.FL_GAS_DISC_MONTH       == fgi.FL_GAS_DISC_MONTH       &&
                                                                                             f.FL_GAS_DISC_DAY         == fgi.FL_GAS_DISC_DAY         &&
                                                                                             f.FL_GAS_DISC_COUNTY_CODE == fgi.FL_GAS_DISC_COUNTY_CODE &&
                                                                                             f.FL_GAS_PERF_1ST_WELL    == fgi.FL_GAS_PERF_1ST_WELL    &&
                                                                                             f.FL_TYPE_FIELD_CODE      == fgi.FL_TYPE_FIELD_CODE      &&
                                                                                             f.FL_OFFSHORE_CODE        == fgi.FL_OFFSHORE_CODE);

                                    if(fgi_query != null)
                                    {
                                        if(fi != null)
                                        {
                                            fi.FieldGasInformation = fgi_query;
                                        }

                                        FieldGasInformationTable.Update(fgi_query);
                                    }
                                    else
                                    {
                                        Console.WriteLine("FieldGasInformationTable.Add(fgi));");
                                    }

                                    break;
                                }
                                case 18:
                                {
                                    fc = new Field49BCalculations(template.Record);

                                    fc_query = Field49BCalculationsTable.FirstOrDefault(f => f.FL_RRCID_DETERMINING_WELL == fc.FL_RRCID_DETERMINING_WELL                  &&
                                                                                             Nullable.Equals(f.FL_G_1_GAS_GRAVITY,         fc.FL_G_1_GAS_GRAVITY)         &&
                                                                                             Nullable.Equals(f.FL_AVG_RESERVOIR_BHP,       fc.FL_AVG_RESERVOIR_BHP)       &&
                                                                                             Nullable.Equals(f.FL_AVG_RESERVOIR_BH_TEMP,   fc.FL_AVG_RESERVOIR_BH_TEMP)   &&
                                                                                             Nullable.Equals(f.FL_FORMATION_VOLUME_FACTOR, fc.FL_FORMATION_VOLUME_FACTOR) &&
                                                                                             Nullable.Equals(f.FL_SOLUTION_GAS_OIL_RATIO,  fc.FL_SOLUTION_GAS_OIL_RATIO)  &&
                                                                                             Nullable.Equals(f.FL_DEVIATION_FACTOR,        fc.FL_DEVIATION_FACTOR));

                                    if(fc_query != null)
                                    {
                                        if(fi != null)
                                        {
                                            fi.Field49BCalculations = fc_query;
                                        }

                                        Field49BCalculationsTable.Update(fc_query);
                                    }
                                    else
                                    {
                                        Console.WriteLine("Field49BCalculationsTable.Add(fc);");
                                    }

                                    break;
                                }
                            }

                            offset += 240;
                        }

                        //UnmanagedMemoryStream source = srcMM.AsStream();

                        //using(BinaryReader sr = new BinaryReader(source))
                        //{
                        //    char[] buffer = new char[2];

                        //    sr.Read(buffer, 0, 2);

                        //    short recordkey = short.Parse(buffer);

                        //    buffer = new char[140];

                        //    sr.Read(buffer, 2, 140);

                        //    string record = new string(buffer);

                        //}
                        //ReadOnlySpan<sbyte> srcSpan = new ReadOnlySpan<sbyte>(source.GetPointer<sbyte>(), 140);

                        //FieldInformation fi = new FieldInformation(source);
                    }

                    transaction.Commit();

                    SaveChanges();
                }
            }
        }

    }
}
