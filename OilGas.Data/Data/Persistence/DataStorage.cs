using System;
using System.IO;
using System.Runtime.CompilerServices;

namespace OilGas.Data
{
    internal abstract class HomeDataStorage
    {
        internal static readonly string HomePath = GetHomePath();

        //internal static readonly string HomeDatabasePath = GetDbPath(HomePath);

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        internal static bool IsValidPath(string path)
        {
            if(string.IsNullOrEmpty(path) || !Directory.Exists(path))
            {
                return false;
            }

            return true;
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        internal static string GetHomePath()
        {
            string homePath = Environment.GetEnvironmentVariable("HOME");

            if(IsValidPath(homePath))
            {
                return homePath;
            }

            homePath = Path.Combine(Environment.GetEnvironmentVariable("HOMEDRIVE"),
                                    Environment.GetEnvironmentVariable("HOMEPATH"));

            if(IsValidPath(homePath))
            {
                return homePath;
            }

            homePath = Environment.GetEnvironmentVariable("USERPROFILE");

            if(IsValidPath(homePath))
            {
                return homePath;
            }

            homePath = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile);

            if(IsValidPath(homePath))
            {
                return homePath;
            }

            homePath = Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData);

            if(IsValidPath(homePath))
            {
                return homePath;
            }

            return homePath;
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        internal static string GetDbPath(string homePath,
                                         string fileName)
        {
            return Path.Combine(homePath,
                                fileName);
        }
    }

    public class DataStorage
    {
        public string FileName { get { return Path.GetFileName(FullPath); } }

        public string DirectoryPath { get { return Path.GetDirectoryName(FullPath); } }

        public string FullPath { get; }

        public string ConnectionString { get; }

        //public DataStorage()
        //{
        //    FullPath = HomeDataStorage.HomeDatabasePath;
        //}

        private static string EnsureFileName(string fileName)
        {
            return fileName.EndsWith(".db") ? fileName : fileName + ".db";
        }

        public DataStorage()
        {
            FullPath = ":memory:";

            ConnectionString = $@"Data Source={FullPath}";
        }

        public DataStorage(string fileName)
            : this(HomeDataStorage.HomePath,
                   EnsureFileName(fileName))
        {
        }

        //@"Server=.\;Database=Northwind;Trusted_Connection=True;Enlist=False;"

        public DataStorage(string directoryPath,
                           string fileName)
        {
            FullPath = Path.Combine(directoryPath,
                                    EnsureFileName(fileName));

            ConnectionString = $@"Data Source={FullPath}";
        }

        //public DataStorage(SQLiteConnectionStringBuilder builder)
        //{
        //    FullPath = builder.DataSource;
        //
        //    ConnectionString = builder.ConnectionString;
        //}

        public static implicit operator DataStorage(string fullPath)
        {
            return new DataStorage(fullPath);
        }

        public override string ToString()
        {
            return FullPath;
        }
    }
}