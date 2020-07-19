using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading.Tasks;

using Engineering.DataSource;
using Engineering.DataSource.CoordinateSystems;
using Engineering.DataSource.OilGas;
using Engineering.DataSource.Tools;

using Kokkos;

using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.ChangeTracking;
using Microsoft.EntityFrameworkCore.Diagnostics;
using Microsoft.EntityFrameworkCore.Diagnostics.Internal;
using Microsoft.EntityFrameworkCore.Infrastructure;
using Microsoft.EntityFrameworkCore.Metadata;
using Microsoft.EntityFrameworkCore.Utilities;
using Microsoft.Extensions.Caching.Memory;
using Microsoft.Extensions.Logging;

using Npgsql;

using NpgsqlTypes;

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

    /// <summary>
    /// New Mexico https://wwwapps.emnrd.state.nm.us/ocd/ocdpermitting/Data/Wells.aspx
    /// </summary>
    public sealed class OilGasDbContext : DbContext
    {
        public const string DefaultDbName = "C:\\OilGas.db";
        //public const string InMemoryName  = ":memory:";

        private static readonly IMemoryCache _cache = new MemoryCache(new MemoryCacheOptions());

        public DbSet<ShapeFileLocation> ShapeFileLocations { get; set; }

        public DbSet<Company> Companys { get; set; }

        public DbSet<DrillingPermit> DrillingPermits { get; set; }

        public DbSet<DirectionalSurvey> DirectionalSurveys { get; set; }

        public DbSet<Field> Fields { get; set; }

        public DbSet<Lease> Leases { get; set; }

        public DbSet<Location> Locations { get; set; }

        public DbSet<WellboreDetails> WellboreDetails { get; set; }

        public DbSet<CompletionDetails> CompletionDetails { get; set; }

        public DbSet<ReservoirData> ReservoirData { get; set; }

        public DbSet<PerforationInterval> PerforationIntervals { get; set; }

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
            : base(new DbContextOptionsBuilder<OilGasDbContext>().UseLazyLoadingProxies()
                                                                 .UseMemoryCache(_cache)
                                                                 .UseNpgsql(connection)
                                                                 //.EnableSensitiveDataLogging()
                                                                  /*.LogTo(Console.WriteLine, LogLevel.Information)*/
                                                                 .Options)
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
            //NpgsqlConnection connection = new NpgsqlConnection($"Host=timothyrmcnealy.com;Port=5432;Username={Encryption.Username};Password={Encryption.Password};Database=OilGas");

            NpgsqlConnection connection;

            try
            {
                connection = new NpgsqlConnection($"Host=timothyrmcnealy.com;Port=5432;Username={Encryption.Username};Password={Encryption.Password};Database=OilGas");

                connection.Open();
            }
            catch(Exception)
            {
                connection = new NpgsqlConnection("Host=timothyrmcnealy.com;Port=5432;Username=db_user;Password=dbAccess;Database=OilGas");

                connection.Open();
            }

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

        public void CloseConnection()
        {
            Connection.Close();
        }

        //protected override void OnConfiguring(DbContextOptionsBuilder optionsBuilder)
        //{
        //}

        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            modelBuilder.Entity<ShapeFileLocation>().HasIndex(dp => new {dp.Id, dp.Api}).IsCreatedConcurrently().IsUnique();

            modelBuilder.Entity<WellboreDetails>().HasIndex(dp => new {dp.Id}).IsCreatedConcurrently().IsUnique();

            modelBuilder.Entity<CompletionDetails>().HasIndex(dp => new {dp.Id}).IsCreatedConcurrently().IsUnique();

            modelBuilder.Entity<ReservoirData>().HasIndex(dp => new {dp.Id}).IsCreatedConcurrently().IsUnique();

            modelBuilder.Entity<PerforationInterval>().HasIndex(dp => new {dp.Id}).IsCreatedConcurrently();

            modelBuilder.Entity<CumulativeProduction>().HasIndex(dp => new {dp.Id, dp.Date}).IsCreatedConcurrently().IsUnique();

            modelBuilder.Entity<DailyProduction>().HasIndex(dp => new {dp.Id, dp.Date}).IsCreatedConcurrently().IsUnique();

            modelBuilder.Entity<DirectionalSurvey>().HasIndex(dp => new {dp.Id});

            modelBuilder.Entity<DrillingPermit>().HasIndex(dp => new {dp.Id}).IsCreatedConcurrently().IsUnique();

            modelBuilder.Entity<Lease>().HasIndex(dp => new {dp.Number, dp.District}).IsCreatedConcurrently().IsUnique();

            modelBuilder.Entity<Field>().HasIndex(dp => new {dp.Number, dp.District}).IsCreatedConcurrently().IsUnique();

            modelBuilder.Entity<Location>().HasIndex(dp => new {dp.Id}).IsCreatedConcurrently().IsUnique();

            modelBuilder.Entity<GasProperties>().HasIndex(dp => new {dp.Id}).IsCreatedConcurrently().IsUnique();

            modelBuilder.Entity<MonthlyProduction>().HasIndex(dp => new {dp.Id, dp.Date}).IsCreatedConcurrently().IsUnique();

            modelBuilder.Entity<OilProperties>().HasIndex(dp => new {dp.Id}).IsCreatedConcurrently().IsUnique();

            modelBuilder.Entity<Company>().HasIndex(dp => dp.Number).IsCreatedConcurrently().IsUnique();

            modelBuilder.Entity<ReservoirProperties>().HasIndex(dp => new {dp.Id}).IsCreatedConcurrently().IsUnique();

            modelBuilder.Entity<WaterProperties>().HasIndex(dp => new {dp.Id}).IsCreatedConcurrently().IsUnique();

            modelBuilder.Entity<Well>().HasIndex(dp => dp.Api).IsCreatedConcurrently().IsUnique();


            modelBuilder.Entity<ShapeFileLocation>().Property(p => p.Api).HasConversion(v => v.ToString(), v => new ApiNumber(v));
            modelBuilder.Entity<DrillingPermit>().Property(p => p.Api).HasConversion(v => v.ToString(), v => new ApiNumber(v));
            modelBuilder.Entity<Well>().Property(p => p.Api).HasConversion(v => v.ToString(), v => new ApiNumber(v));

            modelBuilder.Entity<CumulativeProduction>().Property(p => p.Date).HasConversion(v => (DateTime)v, v => (ProductionDate)v);
            modelBuilder.Entity<DailyProduction>().Property(p => p.Date).HasConversion(v => (DateTime)v, v => (ProductionDate)v);
            modelBuilder.Entity<MonthlyProduction>().Property(p => p.Date).HasConversion(v => (DateTime)v, v => (ProductionDate)v);

            //modelBuilder.HasPostgresExtension("hstore");
            //HasPostgresExtension("uuid-ossp")
        }

        public override void Dispose()
        {
            base.Dispose();

            if(Connection.State != ConnectionState.Closed)
            {
                Connection.Close();
                Connection.Dispose();
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

            command.CommandText =
                "DROP TABLE \"Well\",\"WaterProperties\",\"ReservoirProperties\",\"Companys\",\"OilProperties\",\"MonthlyProduction\",\"Location\",\"GasProperties\",\"Field\",\"DrillingPermit\",\"DirectionalSurvey\",\"DailyProduction\",\"CumulativeProduction\",\"CompletionDetails\" CASCADE;";

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

        public void LoadRelationalProperties(Well well)
        {
            //Entry(well).Reference(nameof(Well.Company)).Load();
            //Entry(well).Reference(nameof(Well.Field)).Load();
            //Entry(well).Reference(nameof(Well.Lease)).Load();
            //Entry(well).Reference(nameof(Well.Location)).Load();
            //Entry(well).Reference(nameof(Well.WellboreDetails)).Load();

            //Entry(well).Collection(nameof(Well.CompletionDetails)).Load();

            //Entry(well).Collection(nameof(Well.DirectionalSurvey)).Load();
            //Entry(well).Collection(nameof(Well.DailyProduction)).Load();
            //Entry(well).Collection(nameof(Well.MonthlyProduction)).Load();
            //Entry(well).Collection(nameof(Well.CumulativeProduction)).Load();
            //Entry(well).Collection(nameof(Well.ReservoirData)).Load();

            //foreach(ReservoirData reservoirData in well.ReservoirData)
            //{
            //    Entry(reservoirData).Reference(nameof(Engineering.DataSource.OilGas.ReservoirData.GasProperties)).Load();
            //    Entry(reservoirData).Reference(nameof(Engineering.DataSource.OilGas.ReservoirData.OilProperties)).Load();
            //    Entry(reservoirData).Reference(nameof(Engineering.DataSource.OilGas.ReservoirData.WaterProperties)).Load();
            //    Entry(reservoirData).Reference(nameof(Engineering.DataSource.OilGas.ReservoirData.ReservoirProperties)).Load();
            //}
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

                                 int     operatorNumber;
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
                                     wellNumber                 = rows[i][index++];
                                     dist                       = rows[i][index++];
                                     county                     = rows[i][index++];
                                     wellboreProfile            = rows[i][index++];
                                     filingPurpose              = rows[i][index++];
                                     amend                      = rows[i][index++];
                                     totalDepth                 = rows[i][index++];
                                     stackedLateralParentWellDp = rows[i][index++];
                                     currentQueue               = rows[i][index];

                                     operatorNumber = OperatorNumber(operatorNameNumber);

                                     company = companies.FirstOrDefault(o => o.Number == operatorNumber);

                                     entries[i] = new Well(api, leaseName, wellNumber, new Location(county, "Texas"), company);
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

        public void AddRange(List<ShapeFileLocation> locations)
        {
            ShapeFileLocations.AddRange(locations);
        }

        public void ConvertLocations()
        {
            List<ShapeFileLocation> shapeFileLocations = ShapeFileLocations.ToList();

            double surfaceEasting;
            double surfaceNorthing;
            double bottomEasting;
            double bottomNorthing;

            ShapeFileLocation shapeFileLocation;

            for(int i = 0; i < shapeFileLocations.Count(); ++i)
            {
                shapeFileLocation = shapeFileLocations[i];

                if(!shapeFileLocation.SurfaceLatitude83.HasValue  ||
                   !shapeFileLocation.SurfaceLongitude83.HasValue ||
                   !shapeFileLocation.BottomLatitude83.HasValue   ||
                   !shapeFileLocation.BottomLongitude83.HasValue)
                {
                    continue;
                }

                (surfaceEasting, surfaceNorthing) = CoordinateConverter.toWebMercator(shapeFileLocation.SurfaceLongitude83.Value, shapeFileLocation.SurfaceLatitude83.Value);
                (bottomEasting, bottomNorthing)   = CoordinateConverter.toWebMercator(shapeFileLocation.BottomLongitude83.Value,  shapeFileLocation.BottomLatitude83.Value);

                shapeFileLocation.SurfaceEasting83  = surfaceEasting;
                shapeFileLocation.SurfaceNorthing83 = surfaceNorthing;
                shapeFileLocation.BottomEasting83   = bottomEasting;
                shapeFileLocation.BottomNorthing83  = bottomNorthing;

                ShapeFileLocations.Update(shapeFileLocation);
            }

            SaveChanges();
        }

        public void Add(Well well)
        {
            Wells.Add(well);
        }

        public async Task AddAsync(Well well)
        {
            await Wells.AddAsync(well);
        }

        public void AddRange(List<Well> wells)
        {
            Wells.AddRange(wells);
        }

        public async Task AddRangeAsync(List<Well> wells)
        {
            await Wells.AddRangeAsync(wells);
        }

        public void Update(Well well)
        {
            Wells.Update(well);
        }

        public void Remove(Well well)
        {
            Wells.Remove(well);
        }

        public async Task RemoveAsync(Well well)
        {
            await Task.FromResult(Wells.Remove(well));
        }

        public void RemoveRange(List<Well> wells)
        {
            Wells.RemoveRange(wells);
        }
        
        public void Commit()
        {
            SaveChanges();
        }

        public async Task CommitAsync()
        {
            await SaveChangesAsync();
        }

        private IQueryable<Well> GetAllWellsIncluding()
        {
            return Wells.AsQueryable();
        }

        private async Task<IQueryable<Well>> GetAllWellsIncludingAsync()
        {
            return await Task.FromResult(Wells.AsQueryable());
        }
        
        public List<Well> GetAllWells()
        {
            List<Well> wells = GetAllWellsIncluding().ToList();

            return wells;
        }

        public async Task<List<Well>> GetAllWellsAsync()
        {
            List<Well> wells = (await GetAllWellsIncludingAsync()).ToList();
            
            return wells;
        }

        public List<Well> GetWellsByOperator(string name)
        {
            List<Well> wells = GetAllWellsIncluding().Where(w => w.Company.Name.Contains(name)).ToList();

            return wells;
        }

        public async Task<List<Well>> GetWellsByOperatorAsync(string name)
        {
            List<Well> wells = (await GetAllWellsIncludingAsync()).Where(w => w.Company.Name.Contains(name)).ToList();

            return wells;
        }

        public List<Well> GetWellsByCounty(string county)
        {
            List<Well> wells = Wells.Where(w => w.Location.County.ToUpper() == county.ToUpper()).ToList();

            return wells;
        }

        public async Task<List<Well>> GetWellsByCountyAsync(string county)
        {
            List<Well> wells = (await GetAllWellsIncludingAsync()).Where(w => w.Location.County == county.ToUpper()).ToList();
            
            return wells;
        }

        public Well GetWellByApi(ApiNumber api)
        {
            Well well = Wells.SingleOrDefault(w => w.Api == api);

            return well;
        }

        public async Task<Well> GetWellByApiAsync(ApiNumber api)
        {
            Well well = await Wells.SingleOrDefaultAsync(w => w.Api == api);

            return well;
        }
        
        public Lease GetLease(long number, int district)
        {
            return Leases.FirstOrDefault(f => f.Number == number && f.District == district);
        }

        public async Task<Lease> GetLeaseAsync(long number, int district)
        {
            return await Leases.FirstOrDefaultAsync(f => f.Number == number && f.District == district);
        }
        
        public Field GetField(long number, int district)
        {
            return Fields.FirstOrDefault(f => f.Number == number && f.District == district);
        }

        public async Task<Field> GetFieldAsync(long number, int district)
        {
            return await Fields.FirstOrDefaultAsync(f => f.Number == number && f.District == district);
        }

        public Field GetField(string name)
        {
            return Fields.FirstOrDefault(f => f.Name.Contains(name));
        }

        public async Task<Field> GetFieldAsync(string name)
        {
            return await Fields.FirstOrDefaultAsync(f => f.Name.Contains(name));
        }

        public Company GetOperator(long number)
        {
            return Companys.FirstOrDefault(o => o.Number == number);
        }

        public async Task<Company> GetOperatorAsync(long number)
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

        public Well AddorUpdate(Well entity)
        {
            Well existing = Wells.FirstOrDefault(e => e.Api == entity.Api);

            if(existing != null)
            {
                return Wells.Update(entity).Entity;
            }

            return Wells.Add(entity).Entity;
        }

        public async Task<Well> AddorUpdateAsync(Well entity)
        {
            Well existing = await Wells.FirstOrDefaultAsync(e => e.Api == entity.Api);

            if(existing != null)
            {
                return Wells.Update(entity).Entity;
            }

            return (await Wells.AddAsync(entity)).Entity;
        }

        #endregion
    }
}
