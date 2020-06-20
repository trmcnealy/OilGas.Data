using System.Runtime.CompilerServices;

using AngleSharp.Dom;
using AngleSharp.Html.Dom;

using Engineering.DataSource;

using Newtonsoft.Json;

namespace OilGas.Data.RRC.Texas
{
    using static HtmlTags;

    public sealed class WellboreQueryColumns
    {
        public ApiNumber ApiNo { get; set; }

        public string ImagesLink { get; set; }

        public string GisViewerLink { get; set; }

        public string CompletionLink { get; set; }

        public string District { get; set; }

        public string LeaseNo { get; set; }

        public string LeaseName { get; set; }

        public string WellNo { get; set; }

        public string FieldName { get; set; }

        public string OperatorName { get; set; }

        public string County { get; set; }

        public string OnSchedule { get; set; }

        public string ApiDepth { get; set; }

        public WellboreQueryColumns(IElement rowNode)
        {
            ApiNo = "42" + rowNode.GetElementByTags(tr.td[1].table.tbody.tr.td[1].a.GetRootParent()).InnerHtml.Trim();

            ImagesLink     = LinkData.FromJson(rowNode.GetElementByTags(tr.td[1].table.tbody.tr.td[2].select.option[2].GetRootParent()).GetAttribute("value")).Url;
            GisViewerLink  = LinkData.FromJson(rowNode.GetElementByTags(tr.td[1].table.tbody.tr.td[2].select.option[3].GetRootParent()).GetAttribute("value")).Url;
            CompletionLink = LinkData.FromJson(rowNode.GetElementByTags(tr.td[1].table.tbody.tr.td[2].select.option[4].GetRootParent()).GetAttribute("value")).Url;

            District     = rowNode.GetElementByTags(tr.td[2].a.GetRootParent())?.InnerHtml.Trim();
            LeaseNo      = rowNode.GetElementByTags(tr.td[3].table.tbody.tr.td[1].a.GetRootParent())?.InnerHtml.Trim();
            LeaseName    = rowNode.GetElementByTags(tr.td[4].GetRootParent())?.InnerHtml.Trim();
            WellNo       = rowNode.GetElementByTags(tr.td[5].GetRootParent())?.InnerHtml.Trim();
            FieldName    = rowNode.GetElementByTags(tr.td[6].a.GetRootParent())?.InnerHtml.Trim();
            OperatorName = rowNode.GetElementByTags(tr.td[7].a.GetRootParent())?.InnerHtml.Trim();
            County       = rowNode.GetElementByTags(tr.td[8].a.GetRootParent())?.InnerHtml.Trim();
            OnSchedule   = rowNode.GetElementByTags(tr.td[9].GetRootParent())?.InnerHtml.Trim();
            ApiDepth     = rowNode.GetElementByTags(tr.td[10].GetRootParent())?.InnerHtml.Trim();
        }

        public override string ToString()
        {
            return $"{ApiNo} {District} {LeaseNo} {LeaseName} {WellNo} {FieldName} {OperatorName} {County} {OnSchedule} {ApiDepth}";
        }

        public class LinkData
        {
            [JsonProperty("url", Required = Required.Always)]
            public string Url { get; set; }

            [JsonProperty("newWindow", Required = Required.Always)]
            public bool NewWindow { get; set; }

            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            public static LinkData FromJson(string json) => JsonConvert.DeserializeObject<LinkData>(json, Converter.Settings);
        }
    }

    public sealed class WellboreQueryData
    {
        public string LeaseDetailAction { get; set; }

        public WellboreQueryColumns Columns { get; set; }

        public override string ToString()
        {
            return Columns.ToString();
        }
    }

    public sealed class CompletionsQueryData
    {
        public CompletionsQueryColumns Columns { get; set; }

        public override string ToString()
        {
            return Columns.ToString();
        }
    }

    public sealed class DirectionalSurveyQueryData
    {
        public DirectionalSurveysColumns Columns { get; set; }

        public override string ToString()
        {
            return Columns.ToString();
        }
    }

    public sealed class LeaseQueryData
    {
        public ApiNumber Api { get; set; }

        public string DistrictNo { get; set; }

        public string LeaseNo { get; set; }

        public LeaseQueryData(ApiNumber api,
                              string districtNo,
                              string leaseNo)
        {
            Api = api;
            DistrictNo = districtNo;
            LeaseNo = leaseNo;
        }
    }

    public sealed class LeaseDetailQueryData
    {
        public LeaseType LeaseType { get; set; }
    }

    public sealed class SpecificLeaseProductionQueryData
    {
        public ApiNumber Api { get; set; }

        public string Date { get; set; }

        public string OIL_BBL_Production { get; set; }

        public string OIL_Disposition { get; set; }

        public string Casinghead_MCF_Production { get; set; }

        public string Casinghead_Disposition { get; set; }

        public string Operator_Name { get; set; }

        public string Operator_No { get; set; }

        public string Field_Name { get; set; }

        public string Field_No { get; set; }

        public SpecificLeaseProductionQueryData(ApiNumber api,
                                                string[]  values)
        {
            Api = api;

            uint index = 0;
            Date                      = values[index++];
            OIL_BBL_Production        = values[index++];
            OIL_Disposition           = values[index++];
            Casinghead_MCF_Production = values[index++];
            Casinghead_Disposition    = values[index++];

            if(values.Length > 5)
            {
                Operator_Name = values[index++];
                Operator_No   = values[index++];
                Field_Name    = values[index++];
                Field_No      = values[index];
            }
            else
            {
                Operator_Name = string.Empty;
                Operator_No   = string.Empty;
                Field_Name    = string.Empty;
                Field_No      = string.Empty;
            }
        }

        public override string ToString()
        {
            return
                $"{Date} {OIL_BBL_Production} {OIL_Disposition} {Casinghead_MCF_Production} {Casinghead_Disposition} {Field_Name} {Operator_Name} {Operator_Name} {Operator_No} {Field_Name} {Field_No}";
        }
    }

    public sealed class CompletionsQueryColumns
    {
        public string TrackingNo { get; set; }

        public string Link { get; set; }

        public string Status { get; set; }

        public string PacketType { get; set; }

        public string APINo { get; set; }

        public string DrillingPermitNo { get; set; }

        public string WellNo { get; set; }

        public string SubmitDate { get; set; }

        public string OperatorNo { get; set; }

        public string OperatorName { get; set; }

        public string LeaseNo { get; set; }

        public string LeaseName { get; set; }

        public CompletionsQueryColumns(IElement rowNode)
        {
            TrackingNo = rowNode.GetElementByTags(tr.td[1].a.GetRootParent()).InnerHtml.Trim();

            Link = rowNode.GetElementByTags(tr.td[1].a.GetRootParent()).GetAttribute("href");

            Status = rowNode.GetElementByTags(tr.td[2].GetRootParent()).InnerHtml.Trim();

            PacketType       = rowNode.GetElementByTags(tr.td[3].GetRootParent()).InnerHtml.Trim();
            APINo            = rowNode.GetElementByTags(tr.td[4].GetRootParent()).InnerHtml.Trim();
            DrillingPermitNo = rowNode.GetElementByTags(tr.td[5].GetRootParent()).InnerHtml.Trim();
            WellNo           = rowNode.GetElementByTags(tr.td[6].GetRootParent()).InnerHtml.Trim();
            SubmitDate       = rowNode.GetElementByTags(tr.td[7].GetRootParent()).InnerHtml.Trim();
            OperatorNo       = rowNode.GetElementByTags(tr.td[8].GetRootParent()).InnerHtml.Trim();
            OperatorName     = rowNode.GetElementByTags(tr.td[9].GetRootParent()).InnerHtml.Trim();
            LeaseNo          = rowNode.GetElementByTags(tr.td[10].GetRootParent()).InnerHtml.Trim();
            LeaseName        = rowNode.GetElementByTags(tr.td[11].GetRootParent()).InnerHtml.Trim();
        }

        public override string ToString()
        {
            return $"{TrackingNo}, {Status}, {PacketType}, {APINo}, {DrillingPermitNo}, {WellNo}, {SubmitDate}, {OperatorNo}, {OperatorName}, {LeaseNo}, {LeaseName}";
        }
    }

    public sealed class DirectionalSurveysColumns
    {
        public string APINo { get; set; }

        public string DrillingPermitNo { get; set; }

        public string Type { get; set; }

        public string LeaseName { get; set; }

        public string From { get; set; }

        public string To { get; set; }

        public string Label { get; set; }

        public string OtherRemarks { get; set; }

        public DirectionalSurveysColumns(IElement rowNode)
        {
            APINo            = rowNode.GetElementByTags(tr.td[2].GetRootParent()).InnerHtml.Trim();
            DrillingPermitNo = rowNode.GetElementByTags(tr.td[3].GetRootParent()).InnerHtml.Trim();
            Type             = rowNode.GetElementByTags(tr.td[4].GetRootParent()).InnerHtml.Trim();
            LeaseName        = rowNode.GetElementByTags(tr.td[5].GetRootParent()).InnerHtml.Trim();
            From             = rowNode.GetElementByTags(tr.td[6].GetRootParent()).InnerHtml.Trim();
            To               = rowNode.GetElementByTags(tr.td[7].GetRootParent()).InnerHtml.Trim();
            Label            = rowNode.GetElementByTags(tr.td[8].GetRootParent()).InnerHtml.Trim();
            OtherRemarks     = rowNode.GetElementByTags(tr.td[9].GetRootParent()).InnerHtml.Trim();
        }

        public override string ToString()
        {
            return $"{APINo}, {DrillingPermitNo}, {Type}, {LeaseName}, {From}, {To}, {Label}, {OtherRemarks}";
        }
    }
}
