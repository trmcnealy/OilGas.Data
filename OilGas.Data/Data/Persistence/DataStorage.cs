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

        //public DataStorage()
        //{
        //    FullPath = HomeDataStorage.HomeDatabasePath;
        //}

        private static string EnsureFileName(string fileName)
        {
            return Path.GetExtension(fileName) != ".db"
                       ? Path.ChangeExtension(fileName,
                                              ".db")
                       : fileName;
        }

        public DataStorage(string fileName)
            : this(HomeDataStorage.HomePath,
                   fileName)
        {
        }

        public DataStorage(string directoryPath,
                           string fileName)
        {
            FullPath = Path.Combine(directoryPath,
                                    fileName);

            if(!File.Exists(FullPath))
            {
                File.Create(FullPath).Close();
            }
        }

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