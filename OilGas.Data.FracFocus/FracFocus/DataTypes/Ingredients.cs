using System;
using System.Data.Common;
using System.Runtime.Serialization;

namespace OilGas.Data.FracFocus
{
    [Serializable]
    [DataContract]
    public class RegistryIngredients : IDataTable<Guid>
    {
        [DataMember]
        public Guid Id { get; set; }

        [DataMember]
        public Guid? KeyPurpose { get; set; }

        [DataMember]
        public string IngredientName { get; set; }

        [DataMember]
        public string CasNumber { get; set; }

        [DataMember]
        public double? PercentHighAdditive { get; set; }

        [DataMember]
        public double? PercentHfJob { get; set; }

        [DataMember]
        public string IngredientComment { get; set; }

        [DataMember]
        public bool IngredientMsds { get; set; }

        [DataMember]
        public double? MassIngredient { get; set; }

        [DataMember]
        public string ClaimantCompany { get; set; }

        [DataMember]
        public Guid KeyDisclosure { get; set; }

        public RegistryIngredients()
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

#nullable enable
        public RegistryIngredients(DbDataReader dbReader)
        {
            int index = 0;
            Id = dbReader.GetGuid(index++);

            KeyPurpose = CheckAndGetValue<Guid?>(dbReader,
                                                 index++);

            IngredientName = CheckAndGetValue<string>(dbReader,
                                                      index++);

            CasNumber = CheckAndGetValue<string>(dbReader,
                                                 index++);

            PercentHighAdditive = CheckAndGetValue<double?>(dbReader,
                                                            index++);

            PercentHfJob = CheckAndGetValue<double?>(dbReader,
                                                     index++);

            IngredientComment = CheckAndGetValue<string?>(dbReader,
                                                         index++);

            IngredientMsds = CheckAndGetValue<bool>(dbReader,
                                                    index++);

            MassIngredient = CheckAndGetValue<double>(dbReader,
                                                      index++);

            ClaimantCompany = CheckAndGetValue<string?>(dbReader,
                                                       index++);

            KeyDisclosure = dbReader.GetGuid(index);
        }
#nullable disable
    }
}