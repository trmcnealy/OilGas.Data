
using System;
using System.IO;
using System.Runtime.CompilerServices;
using System.Runtime.Serialization;
using System.Xml.Serialization;

namespace OilGas.Data
{
    internal abstract class HomeDataStorage
    {
        internal static readonly string HomePath = GetHomePath();

        //internal static readonly string HomeDatabasePath = GetDbPath(HomePath);

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
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
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
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
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#endif
        internal static string GetDbPath(string homePath,
                                         string fileName)
        {
            return Path.Combine(homePath,
                                fileName);
        }
    }
    
    [XmlRoot("DataStorage")]
    [Serializable]
    [DataContract]
    public class DataStorage
    {
        [DataMember]
        [XmlElement]
        public string FileName { get; set; }
        
        [DataMember]
        [XmlElement]
        public string DirectoryPath { get; set; }
        
        [IgnoreDataMember]
        public string FullPath { get { return Path.Combine(DirectoryPath,
                                                           FileName); } }
        
        [DataMember]
        [XmlElement]
        public string ConnectionString { get; set; }

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
            FileName = string.Empty;
            DirectoryPath = string.Empty;

            ConnectionString = $@"Data Source={FileName}";
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
            DirectoryPath = directoryPath;
            FileName = EnsureFileName(fileName);

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