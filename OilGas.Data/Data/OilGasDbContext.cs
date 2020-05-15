using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading.Tasks;

using Engineering.DataSource;
using Engineering.DataSource.OilGas;
using Engineering.DataSource.Tools;

using Kokkos;

using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Caching.Memory;

using Npgsql;

namespace OilGas.Data
{
    public sealed class OilGasDbException : Exception
    {
        public OilGasDbException()
        {
        }

        public OilGasDbException(string message)
            : base(message)
        {
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        internal static void Throw()
        {
            throw new OilGasDbException();
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        internal static void Throw(string message)
        {
            throw new OilGasDbException(message);
        }
    }

    public sealed class OilGasDbContext : DbContext
    {
        public const string DefaultDbName = "R:\\OilGas.db";
        public const string InMemoryName  = ":memory:";

        private static readonly IMemoryCache _cache = new MemoryCache(new MemoryCacheOptions());

        public DbSet<Company> Companys { get; set; }

        public DbSet<DrillingPermit> DrillingPermits { get; set; }

        public DbSet<DirectionalSurvey> DirectionalSurveys { get; set; }

        public DbSet<Field> Fields { get; set; }

        public DbSet<Location> Locations { get; set; }

        public DbSet<CompletionDetails> CompletionDetails { get; set; }

        public DbSet<OilProperties> OilProperties { get; set; }

        public DbSet<GasProperties> GasProperties { get; set; }

        public DbSet<WaterProperties> WaterProperties { get; set; }

        public DbSet<ReservoirProperties> ReservoirProperties { get; set; }

        public DbSet<DailyProduction> DailyProduction { get; set; }

        public DbSet<MonthlyProduction> MonthlyProduction { get; set; }

        public DbSet<CumulativeProduction> CumulativeProduction { get; set; }

        public DbSet<Well> Wells { get; set; }

        public NpgsqlConnection Connection { get; }

        /// <summary>var connString = "Host=myserver;Username=mylogin;Password=mypass;Database=mydatabase";</summary>
        public OilGasDbContext()
            : this(CreateAndOpen())
        {
        }

        public OilGasDbContext(NpgsqlConnection connection)
            : base(new DbContextOptionsBuilder<OilGasDbContext>().UseNpgsql(connection)/*.UseMemoryCache(_cache)*/.Options)
        {
            Connection = connection;

            //command.ExecuteNonQuery();

            //command.CommandText = $"PRAGMA foreign_keys = ON;";

            //command.ExecuteNonQuery();

            //command.CommandText = $"PRAGMA mmap_size={100 * 1024 * 1024};";

            //command.ExecuteNonQuery();

            Database.EnsureCreated();
        }

        private static NpgsqlConnection CreateAndOpen()
        {
            NpgsqlConnection connection = new NpgsqlConnection($"Host=trmdataserver;Port=1433;Username={Encryption.Username};Password={Encryption.Password};Database=OilGas");

            connection.Open();

            //NpgsqlCommand command = connection.CreateCommand();

            //command.CommandText = $"PRAGMA automatic_index = ON;";

            //command.ExecuteNonQuery();

            //command.CommandText = $"PRAGMA foreign_keys = ON;";

            //command.ExecuteNonQuery();

            //command.CommandText = $"PRAGMA mmap_size={100 * 1024 * 1024};";

            //command.ExecuteNonQuery();

            //connection.Close();

            //connection.Open();

            return connection;
        }

        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            modelBuilder.Entity<CompletionDetails>()
                        .HasIndex(dp => new
                         {
                             dp.Id, dp.Api
                         });

            modelBuilder.Entity<CumulativeProduction>()
                        .HasIndex(dp => new
                         {
                             dp.Id,
                             dp.Api,
                             dp.Date
                         });

            modelBuilder.Entity<DailyProduction>()
                        .HasIndex(dp => new
                         {
                             dp.Id,
                             dp.Api,
                             dp.Date
                         });

            modelBuilder.Entity<DirectionalSurvey>()
                        .HasIndex(dp => new
                         {
                             dp.Id, dp.Api
                         });

            modelBuilder.Entity<DrillingPermit>()
                        .HasIndex(dp => new
                         {
                             dp.Id, dp.Api
                         });

            modelBuilder.Entity<Field>()
                        .HasIndex(dp => new
                         {
                             dp.Number
                         });

            modelBuilder.Entity<Location>()
                        .HasIndex(dp => new
                         {
                             dp.Id, dp.Api
                         });

            modelBuilder.Entity<GasProperties>()
                        .HasIndex(dp => new
                         {
                             dp.Id, dp.Api
                         });

            modelBuilder.Entity<MonthlyProduction>()
                        .HasIndex(dp => new
                         {
                             dp.Id,
                             dp.Api,
                             dp.Date
                         });

            modelBuilder.Entity<OilProperties>()
                        .HasIndex(dp => new
                         {
                             dp.Id, dp.Api
                         });

            modelBuilder.Entity<Company>()
                        .HasIndex(dp => new
                         {
                             dp.Number
                         });

            modelBuilder.Entity<ReservoirProperties>()
                        .HasIndex(dp => new
                         {
                             dp.Id, dp.Api
                         });

            modelBuilder.Entity<WaterProperties>()
                        .HasIndex(dp => new
                         {
                             dp.Id, dp.Api
                         });

            modelBuilder.Entity<Well>()
                        .HasIndex(dp => new
                         {
                             dp.Id, dp.Api
                         });

            modelBuilder.Entity<Company>(entity =>
                                          {
                                              entity.HasIndex(e => e.Number).IsUnique();
                                          });

            modelBuilder.Entity<Field>(entity =>
                                       {
                                           entity.HasIndex(e => e.Number).IsUnique();
                                       });

            modelBuilder.Entity<CompletionDetails>().Property(p => p.Api).HasConversion(v => v.ToString(), v => new ApiNumber(v));
            modelBuilder.Entity<CumulativeProduction>().Property(p => p.Api).HasConversion(v => v.ToString(), v => new ApiNumber(v));
            modelBuilder.Entity<DailyProduction>().Property(p => p.Api).HasConversion(v => v.ToString(), v => new ApiNumber(v));
            modelBuilder.Entity<DirectionalSurvey>().Property(p => p.Api).HasConversion(v => v.ToString(), v => new ApiNumber(v));
            modelBuilder.Entity<DrillingPermit>().Property(p => p.Api).HasConversion(v => v.ToString(), v => new ApiNumber(v));
            modelBuilder.Entity<Location>().Property(p => p.Api).HasConversion(v => v.ToString(), v => new ApiNumber(v));
            modelBuilder.Entity<GasProperties>().Property(p => p.Api).HasConversion(v => v.ToString(), v => new ApiNumber(v));
            modelBuilder.Entity<MonthlyProduction>().Property(p => p.Api).HasConversion(v => v.ToString(), v => new ApiNumber(v));
            modelBuilder.Entity<OilProperties>().Property(p => p.Api).HasConversion(v => v.ToString(), v => new ApiNumber(v));
            modelBuilder.Entity<ReservoirProperties>().Property(p => p.Api).HasConversion(v => v.ToString(), v => new ApiNumber(v));
            modelBuilder.Entity<WaterProperties>().Property(p => p.Api).HasConversion(v => v.ToString(), v => new ApiNumber(v));
            modelBuilder.Entity<Well>().Property(p => p.Api).HasConversion(v => v.ToString(), v => new ApiNumber(v));

            modelBuilder.Entity<CumulativeProduction>().Property(p => p.Date).HasConversion(v => v.ToString(), v => new ProductionDate(v));
            modelBuilder.Entity<DailyProduction>().Property(p => p.Date).HasConversion(v => v.ToString(), v => new ProductionDate(v));
            modelBuilder.Entity<MonthlyProduction>().Property(p => p.Date).HasConversion(v => v.ToString(), v => new ProductionDate(v));

            modelBuilder.Entity<CompletionDetails>().HasKey(p => p.Id);
            modelBuilder.Entity<CumulativeProduction>().HasKey(p => p.Id);
            modelBuilder.Entity<DailyProduction>().HasKey(p => p.Id);
            modelBuilder.Entity<DirectionalSurvey>().HasKey(p => p.Id);
            modelBuilder.Entity<DrillingPermit>().HasKey(p => p.Id);
            modelBuilder.Entity<Location>().HasKey(p => p.Id);
            modelBuilder.Entity<GasProperties>().HasKey(p => p.Id);
            modelBuilder.Entity<MonthlyProduction>().HasKey(p => p.Id);
            modelBuilder.Entity<OilProperties>().HasKey(p => p.Id);
            modelBuilder.Entity<ReservoirProperties>().HasKey(p => p.Id);
            modelBuilder.Entity<WaterProperties>().HasKey(p => p.Id);
            modelBuilder.Entity<Well>().HasKey(p => p.Id);

            modelBuilder.Entity<CumulativeProduction>().HasKey(p => p.Id);
            modelBuilder.Entity<DailyProduction>().HasKey(p => p.Id);
            modelBuilder.Entity<MonthlyProduction>().HasKey(p => p.Id);


            //modelBuilder.HasPostgresExtension("hstore");
            //HasPostgresExtension("uuid-ossp")




        }

        public override void Dispose()
        {
            base.Dispose();

            if(Connection.State == ConnectionState.Open)
            {
                Connection.Close();
            }
        }

        //public void AutomaticIndex()
        //{
        //    NpgsqlCommand command = Connection.CreateCommand();

        //    command.CommandText = $"PRAGMA automatic_index = ON;";

        //    command.ExecuteNonQuery();
        //}

        //public void ForeignKey()
        //{
        //    NpgsqlCommand command = Connection.CreateCommand();

        //    command.CommandText = $"PRAGMA foreign_keys = ON;";

        //    command.ExecuteNonQuery();
        //}

        //public void MmapSize()
        //{
        //    NpgsqlCommand command = Connection.CreateCommand();

        //    command.CommandText = $"PRAGMA mmap_size={100 * 1024 * 1024};";

        //    command.ExecuteNonQuery();
        //}

        //public void Compact()
        //{
        //    NpgsqlCommand command = Connection.CreateCommand();

        //    command.CommandText = "PRAGMA auto_vacuum = FULL;";

        //    //VACUUM main INTO filename;

        //    command.ExecuteNonQuery();
        //}

        public void DropAllTables()
        {
            NpgsqlCommand command = Connection.CreateCommand();

            command.CommandText = $"DROP TABLE \"Well\",\"WaterProperties\",\"ReservoirProperties\",\"Companys\",\"OilProperties\",\"MonthlyProduction\",\"Location\",\"GasProperties\",\"Field\",\"DrillingPermit\",\"DirectionalSurvey\",\"DailyProduction\",\"CumulativeProduction\",\"CompletionDetails\" CASCADE;";

            command.ExecuteNonQuery();

            SaveChanges();
        }

        public void Restore()
        {
            //SqlCmd -E -S Server_Name –Q “RESTORE DATABASE [Name_of_Database] FROM DISK=’X:PathToBackupFile[File_Name].bak'”
        }

        public void Backup()
        {
            NpgsqlCommand command = Connection.CreateCommand();

            command.CommandText = $"BACKUP DATABASE databasename TO DISK = \"{DefaultDbName}\";";

            command.ExecuteNonQuery();
        }

        public void LoadDb(string filePath)
        {
            NpgsqlConnection dbFile = new NpgsqlConnection($"Data Source={filePath}");
            dbFile.Open();
            //dbFile.BackupDatabase(Connection);
            dbFile.Close();

            Console.WriteLine($"{filePath} loaded.");
        }

        public void LoadOperatorsCsv(string filePath)
        {
            int num_threads = 4;
            int num_numa    = 1;
            int device_id   = 0;
            int ndevices    = 1;
            int skip_device = 9999;

            InitArguments arguments = new InitArguments(num_threads, num_numa, device_id, ndevices, skip_device, false);

            List<string[]> rows;

            using(ScopeGuard.Get(arguments))
            {
                using(MemoryMapped mm = new MemoryMapped(filePath))
                {
                    MappedCsvReader csvReader = new MappedCsvReader(mm);

                    (_, rows) = csvReader.ReadFile(1);
                }
            }

            Company[] entries = new Company[rows.Count];

            Parallel.ForEach(Partitioner.Create(0, rows.Count),
                             (row,
                              loopState) =>
                             {
                                 string number;
                                 string name;

                                 for(int i = row.Item1; i < row.Item2; i++)
                                 {
                                     number = rows[i][0];
                                     name   = rows[i][1];

                                     entries[i] = new Company(name, number);
                                 }
                             });

            using(NpgsqlTransaction transaction = Connection.BeginTransaction(IsolationLevel.ReadCommitted))
            {
                Companys.AddRange(entries);

                transaction.Commit();

                SaveChanges();
            }
        }

        //@"C:\Users\tehgo\Desktop\DrillingPermits_STX_2000_2020.csv"
        public void LoadDrillingPermitsCsv(string filePath)
        {
            int num_threads = 4;
            int num_numa    = 1;
            int device_id   = 0;
            int ndevices    = 1;
            int skip_device = 9999;

            InitArguments arguments = new InitArguments(num_threads, num_numa, device_id, ndevices, skip_device, false);

            List<string[]> rows;

            using(ScopeGuard.Get(arguments))
            {
                using(MemoryMapped mm = new MemoryMapped(filePath))
                {
                    MappedCsvReader csvReader = new MappedCsvReader(mm);

                    (_, rows) = csvReader.ReadFile(1);
                }
            }

            int OperatorNumber(string operatorNameNumber)
            {
                operatorNameNumber = operatorNameNumber.Substring(0, operatorNameNumber.Length - 1);
                int last = operatorNameNumber.LastIndexOf("(", StringComparison.InvariantCulture);

                return int.Parse(operatorNameNumber.Substring(last + 1));
            }

            Well[] entries = new Well[rows.Count];

            List<Company> companies = Companys.ToList();

            Parallel.ForEach(Partitioner.Create(0, rows.Count),
                             (row,
                              loopState) =>
                             {
                                 int index;

                                 string    statusDate;
                                 int?      status;
                                 ApiNumber api;
                                 string    operatorNameNumber;
                                 string    leaseName;
                                 string    wellNumber;
                                 string    dist;
                                 string    county;
                                 string    wellboreProfile;
                                 string    filingPurpose;
                                 string    amend;
                                 string    totalDepth;
                                 string    stackedLateralParentWellDp;
                                 string    currentQueue;

                                 int      operatorNumber;
                                 Company company;

                                 //for(int i = 0; i < rows.Count; i++)
                                 for(int i = row.Item1; i < row.Item2; i++)
                                 {
                                     index = 0;

                                     statusDate                 = rows[i][index++];
                                     status                     = int.Parse(rows[i][index++]);
                                     api                        = new ApiNumber("42" + rows[i][index++]);
                                     operatorNameNumber         = rows[i][index++];
                                     leaseName                  = rows[i][index++];
                                     wellNumber = rows[i][index++];
                                     dist                       = rows[i][index++];
                                     county                     = rows[i][index++];
                                     wellboreProfile            = rows[i][index++];
                                     filingPurpose              = rows[i][index++];
                                     amend                      = rows[i][index++];
                                     totalDepth                 = rows[i][index++];
                                     stackedLateralParentWellDp = rows[i][index++];
                                     currentQueue               = rows[i][index];

                                     operatorNumber = OperatorNumber(operatorNameNumber);

                                     company      = companies.FirstOrDefault(o => o.Number == operatorNumber);

                                     entries[i] = new Well(api, leaseName, wellNumber, new Location(api, county, "Texas"), company);
                                 }
                             });

            using(NpgsqlTransaction transaction = Connection.BeginTransaction(IsolationLevel.ReadCommitted))
            {
                Wells.AddRange(entries);

                transaction.Commit();

                SaveChanges();
            }
        }

        public void LoadFracFocusRegistryCsv(string filePath)
        {
            //CsvReader csvReader = new CsvReader(File.ReadAllBytes(filePath));

            //(List<string[]> header, List<string[]> rows) = csvReader.ReadFile(1);

            //Registry[] entries = new Registry[rows.Count];

            //Parallel.ForEach(Partitioner.Create(0, rows.Count),
            //                 (row,
            //                  loopState) =>
            //                 {
            //                     for(int i = row.Item1; i < row.Item2; i++)
            //                     {
            //                         entries[i] = new Registry(rows[i]);
            //                     }
            //                 });

            //using(NpgsqlTransaction transaction = Connection.BeginTransaction(IsolationLevel.ReadCommitted))
            //{
            //    FracFocusRegistry.AddRange(entries);

            //    transaction.Commit();

            //    SaveChanges();
            //}
        }

        public void Add(Well well)
        {
            Wells.Add(well);

            SaveChanges();
        }

        public async Task AddAsync(Well well)
        {
            await Wells.AddAsync(well);

            await SaveChangesAsync();
        }

        public void AddRange(List<Well> wells)
        {
            Wells.AddRange(wells);

            SaveChanges();
        }

        public async Task AddRangeAsync(List<Well> wells)
        {
            await Wells.AddRangeAsync(wells);

            await SaveChangesAsync();
        }

        public void Update(Well well)
        {
            Wells.Update(well);

            SaveChanges();
        }

        public void Commit()
        {
            SaveChanges();
        }

        public async Task CommitAsync()
        {
            await SaveChangesAsync();
        }

        public List<Well> GetAllWellsIncluding()
        {
            return Wells.Include(wp => wp.Location)
                        .Include(wp => wp.Company)
                        .Include(wp => wp.Field)
                        .Include(wp => wp.CompletionDetails)
                        .Include(wp => wp.DirectionalSurvey)
                        .Include(wp => wp.GasProperties)
                        .Include(wp => wp.OilProperties)
                        .Include(wp => wp.WaterProperties)
                        .Include(wp => wp.ReservoirProperties)
                        .Include(wp => wp.DailyProduction)
                        .Include(wp => wp.MonthlyProduction)
                        .Include(wp => wp.CumulativeProduction)
                        .ToList();
        }

        public List<Well> GetWellsByOperator(string name)
        {
            List<Well> wells = Companys.Include(wp => wp.Wells).FirstOrDefault(w => w.Name == name)?.Wells;

            return wells;
        }

        public async Task<List<Well>> GetWellsByOperatorAsync(string name)
        {
            List<Well> wells = await Companys.Include(wp => wp.Wells).FirstOrDefaultAsync(w => w.Name == name).Select(o => o.Wells);

            return wells;
        }

        public Well GetWellByApi(ApiNumber api)
        {
            Well well = Wells.Include(wp => wp.Location)
                             .Include(wp => wp.Company)
                             .Include(wp => wp.Field)
                             .Include(wp => wp.CompletionDetails)
                             .Include(wp => wp.DirectionalSurvey)
                             .Include(wp => wp.GasProperties)
                             .Include(wp => wp.OilProperties)
                             .Include(wp => wp.WaterProperties)
                             .Include(wp => wp.ReservoirProperties)
                             .Include(wp => wp.DailyProduction)
                             .Include(wp => wp.MonthlyProduction)
                             .Include(wp => wp.CumulativeProduction)
                             .FirstOrDefault(w => w.Api == api);

            return well;
        }

        public async Task<Well> GetWellByApiAsync(ApiNumber api)
        {
            Well well = await Wells.Include(wp => wp.Location)
                                   .Include(wp => wp.Company)
                                   .Include(wp => wp.Field)
                                   .Include(wp => wp.CompletionDetails)
                                   .Include(wp => wp.DirectionalSurvey)
                                   .Include(wp => wp.GasProperties)
                                   .Include(wp => wp.OilProperties)
                                   .Include(wp => wp.WaterProperties)
                                   .Include(wp => wp.ReservoirProperties)
                                   .Include(wp => wp.DailyProduction)
                                   .Include(wp => wp.MonthlyProduction)
                                   .Include(wp => wp.CumulativeProduction)
                                   .FirstOrDefaultAsync(w => w.Api == api);

            return well;
        }

        public Field GetField(int number)
        {
            return Fields.FirstOrDefault(f => f.Number == number);
        }

        public async Task<Field> GetFieldAsync(int number)
        {
            return await Fields.FirstOrDefaultAsync(f => f.Number == number);
        }

        public Field GetField(string name)
        {
            return Fields.FirstOrDefault(f => f.Name.Contains(name));
        }

        public async Task<Field> GetFieldAsync(string name)
        {
            return await Fields.FirstOrDefaultAsync(f => f.Name.Contains(name));
        }

        public Company GetOperator(int number)
        {
            return Companys.FirstOrDefault(o => o.Number == number);
        }

        public async Task<Company> GetOperatorAsync(int number)
        {
            return await Companys.FirstOrDefaultAsync(o => o.Number == number);
        }

        public Company GetOperator(string name)
        {
            return Companys.FirstOrDefault(f => f.Name.Contains(name));
        }

        public async Task<Company> GetOperatorAsync(string name)
        {
            return await Companys.FirstOrDefaultAsync(f => f.Name.Contains(name));
        }

        #region AddorUpdate
        #region AddorUpdate Operator

        public Company AddorUpdate(Company entity)
        {
            Company existing = Companys.FirstOrDefault(e => e.Number == entity.Number);

            if (existing != null)
            {
                return Companys.Update(entity).Entity;
            }

            return Companys.Add(entity).Entity;
        }

        public async Task<Company> AddorUpdateAsync(Company entity)
        {
            Company existing = await Companys.FirstOrDefaultAsync(e => e.Number == entity.Number);

            if (existing != null)
            {
                return Companys.Update(entity).Entity;
            }

            return (await Companys.AddAsync(entity)).Entity;
        }

        #endregion

        #region AddorUpdate Field

        public Field AddorUpdate(Field entity)
        {
            Field existing = Fields.FirstOrDefault(e => e.Number == entity.Number);

            if (existing != null)
            {
                return Fields.Update(entity).Entity;
            }

            return Fields.Add(entity).Entity;
        }

        public async Task<Field> AddorUpdateAsync(Field entity)
        {
            Field existing = await Fields.FirstOrDefaultAsync(e => e.Number == entity.Number);

            if (existing != null)
            {
                return Fields.Update(entity).Entity;
            }

            return (await Fields.AddAsync(entity)).Entity;
        }

        #endregion

        #region AddorUpdate Location

        public Location AddorUpdate(Location entity)
        {
            Location existing = Locations.FirstOrDefault(e => e.Api == entity.Api);

            if (existing != null)
            {
                return Locations.Update(entity).Entity;
            }

            return Locations.Add(entity).Entity;
        }

        public async Task<Location> AddorUpdateAsync(Location entity)
        {
            Location existing = await Locations.FirstOrDefaultAsync(e => e.Api == entity.Api);

            if (existing != null)
            {
                return Locations.Update(entity).Entity;
            }

            return (await Locations.AddAsync(entity)).Entity;
        }

        #endregion

        #region AddorUpdate CompletionDetails

        public CompletionDetails AddorUpdate(CompletionDetails entity)
        {
            CompletionDetails existing = CompletionDetails.FirstOrDefault(e => e.Api == entity.Api);

            if (existing != null)
            {
                return CompletionDetails.Update(entity).Entity;
            }

            return CompletionDetails.Add(entity).Entity;
        }

        public async Task<CompletionDetails> AddorUpdateAsync(CompletionDetails entity)
        {
            CompletionDetails existing = await CompletionDetails.FirstOrDefaultAsync(e => e.Api == entity.Api);

            if (existing != null)
            {
                return CompletionDetails.Update(entity).Entity;
            }

            return (await CompletionDetails.AddAsync(entity)).Entity;
        }

        #endregion

        #region AddorUpdate OilProperties

        public OilProperties AddorUpdate(OilProperties entity)
        {
            OilProperties existing = OilProperties.FirstOrDefault(e => e.Api == entity.Api);

            if (existing != null)
            {
                return OilProperties.Update(entity).Entity;
            }

            return OilProperties.Add(entity).Entity;
        }

        public async Task<OilProperties> AddorUpdateAsync(OilProperties entity)
        {
            OilProperties existing = await OilProperties.FirstOrDefaultAsync(e => e.Api == entity.Api);

            if (existing != null)
            {
                return OilProperties.Update(entity).Entity;
            }

            return (await OilProperties.AddAsync(entity)).Entity;
        }

        #endregion

        #region AddorUpdate GasProperties

        public GasProperties AddorUpdate(GasProperties entity)
        {
            GasProperties existing = GasProperties.FirstOrDefault(e => e.Api == entity.Api);

            if (existing != null)
            {
                return GasProperties.Update(entity).Entity;
            }

            return GasProperties.Add(entity).Entity;
        }

        public async Task<GasProperties> AddorUpdateAsync(GasProperties entity)
        {
            GasProperties existing = await GasProperties.FirstOrDefaultAsync(e => e.Api == entity.Api);

            if (existing != null)
            {
                return GasProperties.Update(entity).Entity;
            }

            return (await GasProperties.AddAsync(entity)).Entity;
        }

        #endregion

        #region AddorUpdate WaterProperties

        public WaterProperties AddorUpdate(WaterProperties entity)
        {
            WaterProperties existing = WaterProperties.FirstOrDefault(e => e.Api == entity.Api);

            if (existing != null)
            {
                return WaterProperties.Update(entity).Entity;
            }

            return WaterProperties.Add(entity).Entity;
        }

        public async Task<WaterProperties> AddorUpdateAsync(WaterProperties entity)
        {
            WaterProperties existing = await WaterProperties.FirstOrDefaultAsync(e => e.Api == entity.Api);

            if (existing != null)
            {
                return WaterProperties.Update(entity).Entity;
            }

            return (await WaterProperties.AddAsync(entity)).Entity;
        }

        #endregion

        #region AddorUpdate ReservoirProperties

        public ReservoirProperties AddorUpdate(ReservoirProperties entity)
        {
            ReservoirProperties existing = ReservoirProperties.FirstOrDefault(e => e.Api == entity.Api);

            if (existing != null)
            {
                return ReservoirProperties.Update(entity).Entity;
            }

            return ReservoirProperties.Add(entity).Entity;
        }

        public async Task<ReservoirProperties> AddorUpdateAsync(ReservoirProperties entity)
        {
            ReservoirProperties existing = await ReservoirProperties.FirstOrDefaultAsync(e => e.Api == entity.Api);

            if (existing != null)
            {
                return ReservoirProperties.Update(entity).Entity;
            }

            return (await ReservoirProperties.AddAsync(entity)).Entity;
        }

        #endregion

        #region AddorUpdate DailyProduction

        public DailyProduction AddorUpdate(DailyProduction entity)
        {
            DailyProduction existing = DailyProduction.FirstOrDefault(e => e.Api == entity.Api);

            if (existing != null)
            {
                return DailyProduction.Update(entity).Entity;
            }

            return DailyProduction.Add(entity).Entity;
        }

        public async Task<DailyProduction> AddorUpdateAsync(DailyProduction entity)
        {
            DailyProduction existing = await DailyProduction.FirstOrDefaultAsync(e => e.Api == entity.Api);

            if (existing != null)
            {
                return DailyProduction.Update(entity).Entity;
            }

            return (await DailyProduction.AddAsync(entity)).Entity;
        }

        #endregion

        #region AddorUpdate MonthlyProduction

        public MonthlyProduction AddorUpdate(MonthlyProduction entity)
        {
            MonthlyProduction existing = MonthlyProduction.FirstOrDefault(e => e.Api == entity.Api);

            if (existing != null)
            {
                return MonthlyProduction.Update(entity).Entity;
            }

            return MonthlyProduction.Add(entity).Entity;
        }

        public async Task<MonthlyProduction> AddorUpdateAsync(MonthlyProduction entity)
        {
            MonthlyProduction existing = await MonthlyProduction.FirstOrDefaultAsync(e => e.Api == entity.Api);

            if (existing != null)
            {
                return MonthlyProduction.Update(entity).Entity;
            }

            return (await MonthlyProduction.AddAsync(entity)).Entity;
        }

        #endregion

        #region AddorUpdate CumulativeProduction

        public CumulativeProduction AddorUpdate(CumulativeProduction entity)
        {
            CumulativeProduction existing = CumulativeProduction.FirstOrDefault(e => e.Api == entity.Api);

            if (existing != null)
            {
                return CumulativeProduction.Update(entity).Entity;
            }

            return CumulativeProduction.Add(entity).Entity;
        }

        public async Task<CumulativeProduction> AddorUpdateAsync(CumulativeProduction entity)
        {
            CumulativeProduction existing = await CumulativeProduction.FirstOrDefaultAsync(e => e.Api == entity.Api);

            if (existing != null)
            {
                return CumulativeProduction.Update(entity).Entity;
            }

            return (await CumulativeProduction.AddAsync(entity)).Entity;
        }

        #endregion

        #region AddorUpdate Well

        public Well AddorUpdate(Well entity)
        {
            Well existing = Wells.FirstOrDefault(e => e.Api == entity.Api);

            if (existing != null)
            {
                return Wells.Update(entity).Entity;
            }

            return Wells.Add(entity).Entity;
        }

        public async Task<Well> AddorUpdateAsync(Well entity)
        {
            Well existing = await Wells.FirstOrDefaultAsync(e => e.Api == entity.Api);

            if (existing != null)
            {
                return Wells.Update(entity).Entity;
            }

            return (await Wells.AddAsync(entity)).Entity;
        }

        #endregion 
        #endregion
    }
}
