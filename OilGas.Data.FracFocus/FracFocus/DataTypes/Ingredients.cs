using System;

using LiteDB;

namespace OilGas.Data.FracFocus
{
    public class Ingredients : IDataTable
    {
        [BsonId(true)]
        public long Id { get; set; }

        /// <summary>
        /// Key index for the table
        /// </summary>
        public Guid Key { get; set; }

        /// <summary>
        /// Foreign key linking to the RegistryUploadPurpose table.
        /// </summary>
        [BsonRef]
        public PurposeTable PurposeTable { get; set; }

        /// <summary>
        /// Name of the chemical or for Trade Secret chemicals the chemical family name.
        /// </summary>
        public string IngredientName { get; set; }

        /// <summary>
        ///  The Chemical Abstract Service identification number.
        /// </summary>
        public string CasNumber { get; set; }

        /// <summary>
        /// The percent of the ingredient in the Trade Name product in % (Top of the range from MSDS).
        /// </summary>
        public string PercentHighAdditive { get; set; }

        /// <summary>
        /// The amount of the ingredient in the total hydraulic fracturing volume in % by Mass.
        /// </summary>
        public string PercentHfJob { get; set; }

        /// <summary>
        /// Any comments related to the specific ingredient.
        /// </summary>
        public string IngredientComment { get; set; }

        public string IngredientMsds { get; set; }

        public string MassIngredient { get; set; }

        public string ClaimantCompany { get; set; }

        public Guid DisclosureKey { get; set; }

        public Ingredients(PurposeTable         purposeTable,
                           ReadOnlySpan<string> csvData)
        {
            PurposeTable = purposeTable;
            int index = 0;

            Key                 = new Guid(csvData[index++]);
            IngredientName      = csvData[index++];
            CasNumber           = csvData[index++];
            PercentHighAdditive = csvData[index++];
            PercentHfJob        = csvData[index++];
            IngredientComment   = csvData[index++];
            IngredientMsds      = csvData[index++];
            MassIngredient      = csvData[index++];
            ClaimantCompany     = csvData[index++];
            DisclosureKey       = new Guid(csvData[index]);
        }
    }
}