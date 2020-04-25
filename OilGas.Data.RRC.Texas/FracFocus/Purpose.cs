using System;
using System.ComponentModel.DataAnnotations;
using System.Data.Common;
using System.Linq;
using System.Runtime.Serialization;

namespace OilGas.Data.FracFocus
{
    [Serializable]
    [DataContract]
    //[Table("Purpose")]
    public class RegistryPurpose : IDataTable<Guid>
    {
        /// <summary>
        /// Key index for the table
        /// </summary>
        [IgnoreDataMember]
        
        public Guid Id { get; set; }

        [Key]
        public string Uid { get { return Id.ToString(); } }

        /// <summary>
        /// Foreign key linking to the Registry table.
        /// </summary>
        [DataMember]
        public Guid KeyRegistry { get; set; }



        /// <summary>
        /// The name of the product as defined by the supplier.
        /// </summary>
        [DataMember]
        public string TradeName { get; set; }

        /// <summary>
        ///  The name of the company that supplied the product for the hydraulic fracturing job (Usually the service company).
        /// </summary>
        [DataMember]
        public string Supplier { get; set; }

        /// <summary>
        /// The reason the product was used (e.g. Surfactant, Biocide, Proppant).
        /// </summary>
        [DataMember]
        public string Purpose { get; set; }

        [DataMember]
        public bool? SystemApproach { get; set; }

        [DataMember]
        public bool? IsWater { get; set; }

        [DataMember]
        public double? PurposePercentHfJob { get; set; }

        [DataMember]
        public double? IngredientMsds { get; set; }

        public RegistryPurpose()
        {
        }

        private static T CheckAndGetValue<T>(DbDataReader dbReader,
                                             int          index)
        {
            object value = dbReader.GetValue(index);

            if(DBNull.Value == value)
            {
                return default;
            }

            return (T)value;
        }

        private static T CheckAndGetValue<T>(ReadOnlySpan<string> csvData,
                                             int                  index)
        {
            string value = csvData[index];

            if(string.IsNullOrEmpty(value))
            {
                return default;
            }

            if(typeof(T) == typeof(bool) || typeof(T) == typeof(bool?))
            {
                return (T)(object)(value != "False");
            }

            if(typeof(T) == typeof(double) || typeof(T) == typeof(double?))
            {
                return (T)(object)double.Parse(value);
            }

            if(typeof(T) == typeof(DateTime) || typeof(T) == typeof(DateTime?))
            {
                return (T)(object)DateTime.Parse(value);
            }

            if(typeof(T) == typeof(Guid) || typeof(T) == typeof(Guid?))
            {
                return (T)(object)Guid.Parse(value);
            }

            throw new Exception();
        }

        public RegistryPurpose(DbDataReader dbReader)
        {
            int index = 0;

            Id = dbReader.GetGuid(index++);

            KeyRegistry = dbReader.GetGuid(index++);

            TradeName = CheckAndGetValue<string>(dbReader,
                                                 index++);

            Supplier = CheckAndGetValue<string>(dbReader,
                                                index++);

            Purpose = CheckAndGetValue<string>(dbReader,
                                               index++);

            SystemApproach = CheckAndGetValue<bool?>(dbReader,
                                                     index++);

            IsWater = CheckAndGetValue<bool?>(dbReader,
                                              index++);

            PurposePercentHfJob = CheckAndGetValue<double?>(dbReader,
                                                            index++);

            IngredientMsds = CheckAndGetValue<double?>(dbReader,
                                                       index);
        }
    }
}