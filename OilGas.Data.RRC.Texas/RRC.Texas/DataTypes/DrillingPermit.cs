using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Runtime.Serialization;
using System.Text;

using Newtonsoft.Json;
using Newtonsoft.Json.Serialization;

namespace OilGas.Data.RRC.Texas
{
    [Serializable]
    [DataContract]
    public class DrillingPermit
    {
        [DataMember, Key]
        [JsonIgnore]
        public int Id { get; set; }

        [DataMember, Column]
        [JsonProperty("StatusDate", NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string StatusDate{ get; set; }

        [DataMember, Column]
        [JsonProperty("", NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? Status { get; set; }

        [DataMember, Column]
        [JsonProperty("API", NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string Api { get; set; }

        [DataMember, Column]
        [JsonProperty("OperatorNameNumber", NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string OperatorNameNumber { get; set; }

        [DataMember, Column]
        [JsonProperty("LeaseName", NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string LeaseName { get; set; }

        [DataMember, Column]
        [JsonProperty("Well", NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string Well { get; set; }

        [DataMember, Column]
        [JsonProperty("Dist", NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string Dist { get; set; }

        [DataMember, Column]
        [JsonProperty("County", NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string County { get; set; }

        [DataMember, Column]
        [JsonProperty("WellboreProfile", NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string WellboreProfile { get; set; }

        [DataMember, Column]
        [JsonProperty("FilingPurpose", NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string FilingPurpose { get; set; }

        [DataMember, Column]
        [JsonProperty("Amend", NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string Amend { get; set; }

        [DataMember, Column]
        [JsonProperty("TotalDepth", NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string TotalDepth { get; set; }

        [DataMember, Column]
        [JsonProperty("StackedLateralParentWellDp", NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string StackedLateralParentWellDp { get; set; }

        [DataMember, Column]
        [JsonProperty("CurrentQueue", NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string CurrentQueue { get; set; }
        
        public DrillingPermit()
        {
        }

        public DrillingPermit(string statusDate,
                              int? status,
                              ApiNumber api,
                              string operatorNameNumber,
                              string leaseName,
                              string well,
                              string dist,
                              string county,
                              string wellboreProfile,
                              string filingPurpose,
                              string amend,
                              string totalDepth,
                              string stackedLateralParentWellDp,
                              string currentQueue)
        {
            StatusDate = statusDate;
            Status = status;
            Api = api.ToString();
            OperatorNameNumber = operatorNameNumber;
            LeaseName = leaseName;
            Well = well;
            Dist = dist;
            County = county;
            WellboreProfile = wellboreProfile;
            FilingPurpose = filingPurpose;
            Amend = amend;
            TotalDepth = totalDepth;
            StackedLateralParentWellDp = stackedLateralParentWellDp;
            CurrentQueue = currentQueue;
        }
    }
}
