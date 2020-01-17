using System;
using System.Linq;

using LiteDB;

namespace OilGas.Data.FracFocus
{
    public class PurposeTable : IDataTable
    {
        [BsonId(true)]
        public long Id { get; set; }

        /// <summary>
        /// Key index for the table
        /// </summary>
        public Guid Key { get; set; }

        /// <summary>
        /// Foreign key linking to the Registry table.
        /// </summary>
        [BsonRef]
        public Registry Registry { get; set; }

        /// <summary>
        /// The name of the product as defined by the supplier.
        /// </summary>
        public string TradeName { get; set; }

        /// <summary>
        ///  The name of the company that supplied the product for the hydraulic fracturing job (Usually the service company).
        /// </summary>
        public string Supplier { get; set; }

        /// <summary>
        /// The reason the product was used (e.g. Surfactant, Biocide, Proppant).
        /// </summary>
        public string Purpose { get; set; }

        public string SystemApproach { get; set; }

        public bool IsWater { get; set; }

        public string PurposePercentHfJob { get; set; }

        public string PurposeIngredientMsds { get; set; }

        public PurposeTable(Registry             registry,
                            ReadOnlySpan<string> csvData)
        {
            Registry = registry;
            int index = 0;

            Key                   = new Guid(csvData[index++]);
            TradeName             = csvData[index++];
            Supplier              = csvData[index++];
            Purpose               = csvData[index++];
            SystemApproach        = csvData[index++];
            IsWater               = csvData[index++] == "TRUE" ? true : false;
            PurposePercentHfJob   = csvData[index++];
            PurposeIngredientMsds = csvData[index];
        }
    }
}