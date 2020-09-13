using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Data;
using System.Diagnostics;
using System.Linq;
using System.Threading.Tasks;

using Engineering.DataSource;
using Engineering.DataSource.Tools;

using Microsoft.EntityFrameworkCore;

using Npgsql;

namespace OilGas.Data.RRC.Texas
{
    public sealed class TexasAggregateContext : DbContext, IPostgresDbContext
    {
        public const string Host = "timothyrmcnealy.com";
        public const string Port = "15432";

        public DbSet<ProductionAggr> ProductionAggrTable { get; set; }

        public DbSet<WellProductionAggr> WellProductionAggrTable { get; set; }

        public DbSet<WellLocationAggr> WellLocationTable { get; set; }

        public DbSet<LeaseTestAggr> LeaseTestAggrTable { get; set; }

        public DbSet<WellTestAggr> WellTestAggrTable { get; set; }

        public TexasAggregateContext()
            : this(CreateAndOpen())
        {
        }

        public TexasAggregateContext(NpgsqlConnection connection)
            : base(new DbContextOptionsBuilder<TexasAggregateContext>().UseNpgsql(connection).Options)
        {
            Connection = connection;

            Database.EnsureCreated();
        }

        public NpgsqlConnection Connection { get; }

        private static NpgsqlConnection CreateAndOpen()
        {
            NpgsqlConnection connection;

            try
            {
                connection = new NpgsqlConnection($"Host={Host};Port={Port};Username={Encryption.Username};Password={Encryption.Password};Database=TexasAggr");

                connection.Open();
            }
            catch(Exception)
            {
                connection = new NpgsqlConnection($"Host={Host};Port={Port};Username=db_user;Password=dbAccess;Database=TexasAggr");

                connection.Open();
            }

            return connection;
        }

        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            modelBuilder.Entity<ProductionAggr>().HasIndex(dp => new {dp.Api}).IsCreatedConcurrently().IsUnique();

            modelBuilder.Entity<ProductionAggr>().Property(p => p.Api).HasConversion(v => v.ToString(), v => new ApiNumber(v));

            modelBuilder.Entity<ProductionAggr>().HasKey(p => p.Id);

            modelBuilder.Entity<WellProductionAggr>().HasIndex(dp => new {dp.Api, dp.Date}).IsCreatedConcurrently().IsUnique();

            modelBuilder.Entity<WellProductionAggr>().Property(p => p.Api).HasConversion(v => v.ToString(), v => new ApiNumber(v));

            modelBuilder.Entity<WellProductionAggr>().HasKey(p => new {p.Api, p.Date});

            modelBuilder.Entity<WellLocationAggr>().Property(p => p.Api).HasConversion(v => v.ToString(), v => new ApiNumber(v));

            modelBuilder.Entity<WellLocationAggr>().HasKey(p => p.Id);
            
            modelBuilder.Entity<WellTestAggr>().HasKey(p => p.Id);

            modelBuilder.Entity<LeaseTestAggr>().Property(p => p.Api).HasConversion(v => v.ToString(), v => new ApiNumber(v));

            modelBuilder.Entity<LeaseTestAggr>().HasKey(p => p.Id);

            base.OnModelCreating(modelBuilder);

            //modelBuilder.Entity<OgWellCompletionData>()
            //            .HasIndex(dp => new {dp.OIL_GAS_CODE, dp.DISTRICT_NO, dp.LEASE_NO, dp.WELL_NO, dp.API_COUNTY_CODE, dp.API_UNIQUE_NO})
            //            .IsCreatedConcurrently()
            //            .IsUnique();

            //modelBuilder.Entity<OgLeaseCycleData>()
            //            .HasIndex(dp => new {dp.OIL_GAS_CODE, dp.DISTRICT_NO, dp.LEASE_NO, dp.CYCLE_YEAR_MONTH, dp.OPERATOR_NO})
            //            .IsCreatedConcurrently()
            //            .IsUnique();

            //modelBuilder.Entity<OgFieldCycle>()
            //            .HasIndex(dp => new
            //             {
            //                 dp.DISTRICT_NO,
            //                 dp.FIELD_NO,
            //                 dp.CYCLE_YEAR,
            //                 dp.CYCLE_MONTH,
            //                 dp.FIELD_OIL_PROD_VOL,
            //                 dp.FIELD_GAS_PROD_VOL,
            //                 dp.FIELD_COND_PROD_VOL,
            //                 dp.FIELD_CSGD_PROD_VOL
            //             })
            //            .IsCreatedConcurrently()
            //            .IsUnique();

            //modelBuilder.Entity<OgFieldDw>().HasIndex(dp => new {dp.FIELD_NO, dp.FIELD_NAME, dp.DISTRICT_NO}).IsCreatedConcurrently().IsUnique();
            //modelBuilder.Entity<OgOperatorDw>().HasIndex(dp => new {dp.OPERATOR_NO}).IsCreatedConcurrently().IsUnique();
            //modelBuilder.Entity<OgRegulatoryLeaseDw>().HasIndex(dp => new {dp.OIL_GAS_CODE, dp.DISTRICT_NO, dp.LEASE_NO}).IsCreatedConcurrently().IsUnique();

            //modelBuilder.Entity<FieldInformation>();
            //modelBuilder.Entity<FieldGasInformation>();
            //modelBuilder.Entity<Field49BCalculations>();
        }

        public void Commit()
        {
            SaveChanges();
        }

        public async Task CommitAsync()
        {
            await SaveChangesAsync();
        }

        public void BuildProductionAggrTable()
        {
            using TexasDumpDbContext tdc = new TexasDumpDbContext();

            NpgsqlCommand command = tdc.Connection.CreateCommand();

            command.CommandText = "SELECT "                                                                                                                                       +
                                  "\"OgFieldDw\".\"FIELD_CLASS\" AS \"FIELD_CLASS\","                                                                                             +
                                  "\"OgRegulatoryLeaseDw\".\"OIL_GAS_CODE\" AS \"OIL_GAS_CODE\","                                                                                 +
                                  "\"OgRegulatoryLeaseDw\".\"DISTRICT_NO\" AS \"DISTRICT_NO\","                                                                                   +
                                  "\"OgRegulatoryLeaseDw\".\"DISTRICT_NAME\" AS \"DISTRICT_NAME\","                                                                               +
                                  "\"OgRegulatoryLeaseDw\".\"LEASE_NO\" AS \"LEASE_NO\","                                                                                         +
                                  "\"OgRegulatoryLeaseDw\".\"LEASE_NAME\" AS \"LEASE_NAME\","                                                                                     +
                                  "\"OgWellCompletionData\".\"WELL_NO\" AS \"WELL_NO\","                                                                                          +
                                  "\"OgRegulatoryLeaseDw\".\"FIELD_NO\" AS \"FIELD_NO\","                                                                                         +
                                  "\"OgRegulatoryLeaseDw\".\"FIELD_NAME\" AS \"FIELD_NAME\","                                                                                     +
                                  "\"OgRegulatoryLeaseDw\".\"OPERATOR_NO\" AS \"OPERATOR_NO\","                                                                                   +
                                  "\"OgRegulatoryLeaseDw\".\"OPERATOR_NAME\" AS \"OPERATOR_NAME\","                                                                               +
                                  "\"OgWellCompletionData\".\"API_COUNTY_CODE\" AS \"API_COUNTY_CODE\","                                                                          +
                                  "\"OgWellCompletionData\".\"API_UNIQUE_NO\" AS \"API_UNIQUE_NO\" "                                                                              +
                                  "FROM \"OgRegulatoryLeaseDw\" "                                                                                                                 +
                                  "LEFT JOIN \"OgFieldDw\" "                                                                                                                      +
                                  "ON \"OgRegulatoryLeaseDw\".\"FIELD_NO\"=\"OgFieldDw\".\"FIELD_NO\" AND \"OgRegulatoryLeaseDw\".\"DISTRICT_NO\"=\"OgFieldDw\".\"DISTRICT_NO\" " +
                                  "LEFT JOIN \"OgOperatorDw\" "                                                                                                                   +
                                  "ON \"OgRegulatoryLeaseDw\".\"OPERATOR_NO\"=\"OgOperatorDw\".\"OPERATOR_NO\" "                                                                  +
                                  "LEFT JOIN \"OgWellCompletionData\" "                                                                                                           +
                                  "ON \"OgRegulatoryLeaseDw\".\"DISTRICT_NO\"=\"OgWellCompletionData\".\"DISTRICT_NO\" AND \"OgRegulatoryLeaseDw\".\"LEASE_NO\"=\"OgWellCompletionData\".\"LEASE_NO\";";

            List<ApiNumber> apis = ProductionAggrTable.Select(p => p.Api).ToList();

            using(NpgsqlDataReader reader = command.ExecuteReader())
            {
                if(reader.HasRows)
                {
                    long counter = 0;

                    ProductionAggr     productionAggr;
                    HashSet<ApiNumber> productionAggrSet = new HashSet<ApiNumber>(770000);

                    foreach(ApiNumber api in apis)
                    {
                        productionAggrSet.Add(api);
                    }

                    List<ProductionAggr> productionAggrs = new List<ProductionAggr>(770000 - apis.Count);

                    while(reader.Read())
                    {
                        productionAggr = new ProductionAggr(reader);

                        if(!productionAggrSet.Contains(productionAggr.Api))
                        {
                            productionAggrSet.Add(productionAggr.Api);
                            productionAggrs.Add(productionAggr);
                            ++counter;
                        }

                        if(counter % 1000 == 0)
                        {
                            Console.WriteLine($"{counter}");
                        }
                    }

                    ProductionAggrTable.AddRange(productionAggrs);
                    Commit();
                }
                else
                {
                    Console.WriteLine("No rows found.");
                }
            }
        }

        public void FixProductionAggrTable()
        {
            ParallelQuery<OgWellCompletionData> ogWellCompletionDataTable;

            using(TexasDumpDbContext tdc = new TexasDumpDbContext())
            {
                ogWellCompletionDataTable = tdc.OgWellCompletionDataTable.ToList().AsParallel();
            }

            List<ProductionAggr> productionAggrTable = ProductionAggrTable.Where(t => t.WELL_NO == null).ToList();

            Parallel.ForEach(Partitioner.Create(0, productionAggrTable.Count),
                             row =>
                             {
                                 ProductionAggr       productionAggr;
                                 OgWellCompletionData ogWellCompletionData;

                                 for(int i = row.Item1; i < row.Item2; i++)
                                 {
                                     productionAggr = productionAggrTable[i];

                                     ogWellCompletionData = ogWellCompletionDataTable.First(p => p.API_COUNTY_CODE == productionAggr.Api.CountyCode &&
                                                                                                 p.API_UNIQUE_NO   == productionAggr.Api.UniqueWellIdentifier);

                                     productionAggr.WELL_NO = ogWellCompletionData.WELL_NO;
                                 }
                             });

            ProductionAggrTable.UpdateRange(productionAggrTable);
            Commit();
        }

        public void BuildWellProductionAggrTable()
        {
            using TexasDumpDbContext tdc = new TexasDumpDbContext();

            //1-14
            List<ProductionAggr> productionAggrs = ProductionAggrTable.Where(p => p.DISTRICT_NO == 2).ToList();
            //List<ProductionAggr> productionAggrs = new List<ProductionAggr>(2200);

            //ImmutableHashSet<ApiNumber> wellProductionAggrApis = WellProductionAggrTable.Select(o => o.Api).Distinct().ToImmutableHashSet();

            //foreach (ProductionAggr production in productionAggrsFull)
            //{
            //    if(!wellProductionAggrApis.Contains(production.Api))
            //    {
            //        productionAggrs.Add(production);
            //    }
            //}

            ApiNumber api;
            char      code;
            int       DISTRICT_NO;
            long      OPERATOR_NO;
            long      FIELD_NO;
            long      LEASE_NO;

            int counter = 0;

            List<WellProductionAggr>                 wellProductions    = new List<WellProductionAggr>(256000);
            Dictionary<DateTime, WellProductionAggr> wellProductionAggr = new Dictionary<DateTime, WellProductionAggr>(256);

            foreach(ProductionAggr productionAggr in productionAggrs)
            {
                ++counter;

                api         = productionAggr.Api;
                code        = productionAggr.FIELD_CLASS;
                DISTRICT_NO = productionAggr.DISTRICT_NO;
                OPERATOR_NO = productionAggr.OPERATOR_NO;
                FIELD_NO    = productionAggr.FIELD_NO;
                LEASE_NO    = productionAggr.LEASE_NO;

                wellProductionAggr.Clear();

                NpgsqlCommand command = tdc.Connection.CreateCommand();

                command.CommandText = "SELECT "                                            +
                                      "leaseData.\"CYCLE_YEAR\" AS \"YEAR\","              +
                                      "leaseData.\"CYCLE_MONTH\" AS \"MONTH\","            +
                                      "leaseData.\"LEASE_GAS_PROD_VOL\" AS \"GAS_VOL\","   +
                                      "leaseData.\"LEASE_COND_PROD_VOL\" AS \"COND_VOL\"," +
                                      "leaseData.\"LEASE_OIL_PROD_VOL\" AS \"OIL_VOL\" "   +
                                      "FROM \"OgLeaseCycleData\" AS leaseData "            +
                                      $"WHERE leaseData.\"OIL_GAS_CODE\"='O' AND leaseData.\"DISTRICT_NO\"={DISTRICT_NO} AND leaseData.\"LEASE_NO\"={LEASE_NO};";

                using(NpgsqlDataReader reader = command.ExecuteReader())
                {
                    if(reader.HasRows)
                    {
                        WellProductionAggr gasWellProductionAggr;

                        while(reader.Read())
                        {
                            gasWellProductionAggr = new WellProductionAggr(api, reader);
                            wellProductionAggr.Add(gasWellProductionAggr.Date, gasWellProductionAggr);
                        }
                    }
                }

                command.CommandText = "SELECT "                                            +
                                      "leaseData.\"CYCLE_YEAR\" AS \"YEAR\","              +
                                      "leaseData.\"CYCLE_MONTH\" AS \"MONTH\","            +
                                      "leaseData.\"LEASE_GAS_PROD_VOL\" AS \"GAS_VOL\","   +
                                      "leaseData.\"LEASE_COND_PROD_VOL\" AS \"COND_VOL\"," +
                                      "leaseData.\"LEASE_OIL_PROD_VOL\" AS \"OIL_VOL\" "   +
                                      "FROM \"OgLeaseCycleData\" AS leaseData "            +
                                      $"WHERE leaseData.\"OIL_GAS_CODE\"='G' AND leaseData.\"DISTRICT_NO\"={DISTRICT_NO} AND leaseData.\"LEASE_NO\"={LEASE_NO};";

                using(NpgsqlDataReader reader = command.ExecuteReader())
                {
                    if(reader.HasRows)
                    {
                        WellProductionAggr oilWellProductionAggr;

                        while(reader.Read())
                        {
                            oilWellProductionAggr = new WellProductionAggr(api, reader);

                            if(wellProductionAggr.TryGetValue(oilWellProductionAggr.Date, out WellProductionAggr gasWellProductionAggr))
                            {
                                gasWellProductionAggr.GAS_VOL                  += oilWellProductionAggr.GAS_VOL;
                                gasWellProductionAggr.COND_VOL                 += oilWellProductionAggr.COND_VOL;
                                gasWellProductionAggr.OIL_VOL                  += oilWellProductionAggr.OIL_VOL;
                                wellProductionAggr[gasWellProductionAggr.Date] =  gasWellProductionAggr;
                            }
                            else
                            {
                                wellProductionAggr.Add(oilWellProductionAggr.Date, oilWellProductionAggr);
                            }
                        }
                    }
                }

                Console.WriteLine($"{api}");

                wellProductions.AddRange(wellProductionAggr.Values);

                if(counter % 1000 == 0)
                {
                    using(NpgsqlTransaction transaction = Connection.BeginTransaction(IsolationLevel.ReadCommitted))
                    {
                        WellProductionAggrTable.AddRange(wellProductions);

                        transaction.Commit();

                        SaveChanges();
                    }

                    wellProductions.Clear();
                }
            }

            using(NpgsqlTransaction transaction = Connection.BeginTransaction(IsolationLevel.ReadCommitted))
            {
                WellProductionAggrTable.AddRange(wellProductions);

                transaction.Commit();

                SaveChanges();
            }
        }

        public void BuildLocationTable()
        {
            using TexasDumpDbContext tdc = new TexasDumpDbContext();

            NpgsqlCommand command = tdc.Connection.CreateCommand();

            command.CommandText = "SELECT "                                     +
                                  "\"WellS\".\"API\" AS \"API\","               +
                                  "\"WellS\".\"LONG27\" AS \"SURFACE_LONG27\"," +
                                  "\"WellS\".\"LAT27\" AS \"SURFACE_LAT27\","   +
                                  "\"WellS\".\"LONG83\" AS \"SURFACE_LONG83\"," +
                                  "\"WellS\".\"LAT83\" AS \"SURFACE_LAT83\","   +
                                  "\"WellB\".\"LONG27\" AS \"BOTTOM_LONG27\","  +
                                  "\"WellB\".\"LAT27\" AS \"BOTTOM_LAT27\","    +
                                  "\"WellB\".\"LONG83\" AS \"BOTTOM_LONG83\","  +
                                  "\"WellB\".\"LAT83\" AS \"BOTTOM_LAT83\" "    +
                                  "FROM \"WellS\" "                             +
                                  "LEFT JOIN \"WellB\" "                        +
                                  "ON \"WellS\".\"SURFACE_ID\"=\"WellB\".\"SURFACE_ID\" ";

            using(NpgsqlDataReader reader = command.ExecuteReader())
            {
                if(reader.HasRows)
                {
                    long counter = 0;

                    List<WellLocationAggr> entities = new List<WellLocationAggr>(770000);

                    while(reader.Read())
                    {
                        entities.Add(new WellLocationAggr(reader));
                        ++counter;

                        if(counter % 1000 == 0)
                        {
                            Console.WriteLine($"{counter}");
                        }
                    }

                    using(NpgsqlTransaction transaction = Connection.BeginTransaction(IsolationLevel.ReadCommitted))
                    {
                        WellLocationTable.AddRange(entities);

                        transaction.Commit();

                        SaveChanges();
                    }
                }
                else
                {
                    Console.WriteLine("No rows found.");
                }
            }
        }

        private static List<T> QueryResult<T>(IPostgresDbContext context,
                                              string             queryString)
        {
            NpgsqlCommand command = context.Connection.CreateCommand();

            command.CommandText = queryString;

            List<T> rows = new List<T>(100);

            using(NpgsqlDataReader reader = command.ExecuteReader())
            {
                if(reader.HasRows)
                {
                    while(reader.Read())
                    {
                        T row = IlActivator<Func<NpgsqlDataReader, T>>.GetDelegate()(reader);

                        if(row != null)
                        {
                            rows.Add(row);
                        }
                    }
                }

                Console.WriteLine("No rows found.");
            }

            return rows;
        }

        public void BuildWellTestAggrTable()
        {
            using TexasDumpDbContext tdc = new TexasDumpDbContext();

            NpgsqlCommand command = tdc.Connection.CreateCommand();

            command.CommandText = "SELECT "                                                  +
                                  "\"FieldInformation\".\"FL_DISTRICT\","                    +
                                  "\"FieldInformation\".\"FL_FIELD_NUMBER\","                +
                                  "\"FieldInformation\".\"FL_RESERVOIR_NUMBER\","            +
                                  "\"FieldInformation\".\"FL_NAME\","                        +
                                  "\"FieldInformation\".\"FL_FIELD_CLASS\","                 +
                                  "\"FieldInformation\".\"FL_RESERVOIR_NAME\","              +
                                  "\"FieldInformation\".\"FL_FORMATION_COMPOSITION\","       +
                                  "\"FieldInformation\".\"FL_OIL_DISC_WELL_GRAVITY\","       +
                                  "\"Field49BCalculations\".\"FL_RRCID_DETERMINING_WELL\","  +
                                  "\"Field49BCalculations\".\"FL_G_1_GAS_GRAVITY\","         +
                                  "\"Field49BCalculations\".\"FL_AVG_RESERVOIR_BHP\","       +
                                  "\"Field49BCalculations\".\"FL_AVG_RESERVOIR_BH_TEMP\","   +
                                  "\"Field49BCalculations\".\"FL_FORMATION_VOLUME_FACTOR\"," +
                                  "\"Field49BCalculations\".\"FL_SOLUTION_GAS_OIL_RATIO\","  +
                                  "\"Field49BCalculations\".\"FL_DEVIATION_FACTOR\","        +
                                  "\"Field49BCalculations\".\"FL_TOP_DAILY_GAS_ALLOW_MCF\" " +
                                  "FROM \"FieldInformation\" "                               +
                                  "LEFT JOIN \"Field49BCalculations\" "                      +
                                  "ON \"FieldInformation\".\"Id\"=\"Field49BCalculations\".\"FieldInformationId\" ";

            using(NpgsqlDataReader reader = command.ExecuteReader())
            {
                if(reader.HasRows)
                {
                    long counter = 0;

                    List<WellTestAggr> wellTestAggr = new List<WellTestAggr>(1000000);

                    while(reader.Read())
                    {
                        wellTestAggr.Add(new WellTestAggr(reader));
                        ++counter;

                        if(counter % 1000 == 0)
                        {
                            Console.WriteLine($"{counter}");
                        }
                    }

                    WellTestAggrTable.AddRange(wellTestAggr);
                    Commit();
                }
                else
                {
                    Console.WriteLine("No rows found.");
                }
            }
        }

        public void BuildLeaseTestAggrTable()
        {
            using TexasDumpWLDbContext tdc = new TexasDumpWLDbContext();

            //1-14
            List<ProductionAggr> productionAggrs = ProductionAggrTable.Where(p => p.DISTRICT_NO == 2).ToList();

            //List<ProductionAggr> productionAggrs = new List<ProductionAggr>(2200);

            //ImmutableHashSet<ApiNumber> wellProductionAggrApis = WellProductionAggrTable.Select(o => o.Api).Distinct().ToImmutableHashSet();

            //foreach (ProductionAggr production in productionAggrsFull)
            //{
            //    if(!wellProductionAggrApis.Contains(production.Api))
            //    {
            //        productionAggrs.Add(production);
            //    }
            //}

            NpgsqlCommand tdcCommand = tdc.Connection.CreateCommand();
            NpgsqlCommand command    = Connection.CreateCommand();

            ApiNumber     api;
            int           DISTRICT_NO;
            long          LEASE_NO;
            string        WELL_NO;
            LeaseTestAggr leaseTestAggr;

            int counter = 0;

            List<LeaseTestAggr> leaseTestAggrs = new List<LeaseTestAggr>(10000);

            foreach(ProductionAggr productionAggr in productionAggrs)
            {
                ++counter;

                api           = productionAggr.Api;
                DISTRICT_NO   = productionAggr.DISTRICT_NO;
                LEASE_NO      = productionAggr.LEASE_NO;
                WELL_NO       = productionAggr.WELL_NO;
                leaseTestAggr = null;

                tdcCommand.CommandText = string.Format(Queries.GasWellRootQuery, DISTRICT_NO, LEASE_NO, WELL_NO);

                using(NpgsqlDataReader reader = tdcCommand.ExecuteReader())
                {
                    if(reader.HasRows)
                    {
                        while(reader.Read())
                        {
                            leaseTestAggr = new LeaseTestAggr(productionAggr.Api,
                                                              productionAggr.DISTRICT_NO,
                                                              productionAggr.LEASE_NO,
                                                              productionAggr.WELL_NO,
                                                              reader[0].CharValue(),
                                                              null,
                                                              null,
                                                              (float?)reader[4].LongValue(),
                                                              (float?)reader[5].LongValue(),
                                                              null,
                                                              null,
                                                              reader[6].FloatValue(),
                                                              reader[7].FloatValue(),
                                                              (float?)reader[8].LongValue());

                            leaseTestAggr.Id = counter - 1;
                        }
                    }
                }

                command.CommandText = string.Format(Queries.OilWellRootQuery, DISTRICT_NO, LEASE_NO);

                using(NpgsqlDataReader reader = command.ExecuteReader())
                {
                    if(reader.HasRows)
                    {
                        while(reader.Read())
                        {
                            if(leaseTestAggr != null)
                            {
                                leaseTestAggr.FIELD_CLASS             = 'B';
                                leaseTestAggr.RESERVOIR_NUMBER        = leaseTestAggr.RESERVOIR_NUMBER.IfNullOrEqualToReplace(0, reader[1].IntValue());
                                leaseTestAggr.RESERVOIR_NAME          = leaseTestAggr.RESERVOIR_NAME.IfNullReplace(reader[3].StringValue());
                                leaseTestAggr.OIL_GRAVITY             = leaseTestAggr.OIL_GRAVITY.IfNullOrEqualToReplace(0.0f, reader[4].FloatValue());
                                leaseTestAggr.GAS_GRAVITY             = leaseTestAggr.GAS_GRAVITY.IfNullOrEqualToReplace(0.0f, reader[6].FloatValue());
                                leaseTestAggr.AVG_RESERVOIR_BHP       = leaseTestAggr.AVG_RESERVOIR_BHP.IfNullOrEqualToReplace(0.0f, reader[7].FloatValue());
                                leaseTestAggr.AVG_RESERVOIR_BH_TEMP   = leaseTestAggr.AVG_RESERVOIR_BH_TEMP.IfNullOrEqualToReplace(0.0f, reader[8].FloatValue());
                                leaseTestAggr.FORMATION_VOLUME_FACTOR = leaseTestAggr.FORMATION_VOLUME_FACTOR.IfNullOrEqualToReplace(0.0f, reader[9].FloatValue());
                                leaseTestAggr.SOLUTION_GAS_OIL_RATIO  = leaseTestAggr.SOLUTION_GAS_OIL_RATIO.IfNullOrEqualToReplace(0.0f, reader[10].FloatValue());
                            }
                            else
                            {
                                leaseTestAggr = new LeaseTestAggr(productionAggr.Api,
                                                                  productionAggr.DISTRICT_NO,
                                                                  productionAggr.LEASE_NO,
                                                                  productionAggr.WELL_NO,
                                                                  reader[2].CharValue(),
                                                                  reader[1].IntValue(),
                                                                  reader[3].StringValue(),
                                                                  null,
                                                                  reader[7].FloatValue(),
                                                                  reader[8].FloatValue(),
                                                                  reader[9].FloatValue(),
                                                                  reader[6].FloatValue(),
                                                                  reader[4].FloatValue(),
                                                                  reader[10].FloatValue());

                                leaseTestAggr.Id = counter - 1;
                            }
                        }
                    }
                }

                if(leaseTestAggr is null)
                {
                    continue;
                }

                leaseTestAggrs.Add(leaseTestAggr);

                Console.WriteLine($"{api}");

                if(counter % 10000 == 0)
                {
                    using(NpgsqlTransaction transaction = Connection.BeginTransaction(IsolationLevel.ReadCommitted))
                    {
                        LeaseTestAggrTable.AddRange(leaseTestAggrs);

                        transaction.Commit();

                        SaveChanges();
                    }

                    leaseTestAggrs.Clear();
                }
            }

            using(NpgsqlTransaction transaction = Connection.BeginTransaction(IsolationLevel.ReadCommitted))
            {
                LeaseTestAggrTable.AddRange(leaseTestAggrs);

                transaction.Commit();

                SaveChanges();
            }
        }
    }
}
