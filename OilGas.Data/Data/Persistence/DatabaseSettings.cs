using System.Collections.Generic;
using System.Linq;

using LinqToDB.Configuration;

namespace OilGas.Data
{
    public sealed class ConnectionStringSettings : IConnectionStringSettings
    {
        public bool IsGlobal { get { return false; } }

        public string Name { get; set; }

        public string ProviderName { get; set; }

        public string ConnectionString { get; set; }

        public ConnectionStringSettings(string name,
                                        string providerName,
                                        string connectionString)
        {
            Name             = name;
            ProviderName     = providerName;
            ConnectionString = connectionString;
        }
    }

    public sealed class DefaultConnectionSettings : ILinqToDBSettings
    {
        public IEnumerable<IDataProviderSettings> DataProviders { get { return Enumerable.Empty<IDataProviderSettings>(); } }

        public string DefaultConfiguration { get { return LinqToDB.ProviderName.SQLiteClassic; } }

        public string DefaultDataProvider { get { return LinqToDB.ProviderName.SQLite; } }

        public IEnumerable<IConnectionStringSettings> ConnectionStrings
        {
            get
            {
                yield return new ConnectionStringSettings(Name,
                                                          LinqToDB.ProviderName.SQLite,
                                                          ConfigurationString);
            }
        }

        public string Name { get; }

        public string ConfigurationString { get; }

        public DefaultConnectionSettings(string name,
                                         string configurationString)
        {
            Name                = name;
            ConfigurationString = configurationString;
        }
    }
}