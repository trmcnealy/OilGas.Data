using System;
using System.IO;
using System.Runtime.CompilerServices;

namespace OilGas.Data
{
    internal abstract class HomeDataStorage
    {
        internal const string RrcTexasDatabaseName = "OilGas.Data.db";

        internal static readonly string HomePath = GetHomePath();

        internal static readonly string HomeDatabasePath = GetDbPath(HomePath);

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        internal static bool IsValidPath(string path)
        {
            if(string.IsNullOrEmpty(path) || !Directory.Exists(path))
            {
                return false;
            }

            return true;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
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

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        internal static string GetDbPath(string homePath)
        {
            return Path.Combine(homePath,
                                RrcTexasDatabaseName);
        }
    }

    public class DataStorage
    {
        public string FileName { get { return Path.GetFileName(FullPath); } }

        public string DirectoryPath { get { return Path.GetDirectoryName(FullPath); } }

        public string FullPath { get; }

        public DataStorage()
        {
            FullPath = HomeDataStorage.HomeDatabasePath;
        }

        public DataStorage(string fullPath)
        {
            if(!Path.HasExtension(fullPath))
            {
                throw new Exception("Instance path must point to a file.");
            }

            FullPath = Path.GetExtension(fullPath) != ".db"
                           ? Path.ChangeExtension(fullPath,
                                                  ".db")
                           : fullPath;
        }

        public DataStorage(string directoryPath,
                           string fileName)
        {
            FullPath = Path.Combine(directoryPath,
                                    fileName);
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