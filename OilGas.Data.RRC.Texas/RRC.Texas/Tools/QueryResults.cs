using AngleSharp.Dom;

namespace OilGas.Data.RRC.Texas
{
    using static HtmlTags;

    public sealed class WellboreQueryColumns
    {
        public ApiNumber ApiNo { get; set; }

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
            ApiNo        = "42" + rowNode.GetElementByTags(tr.td[1].table.tbody.tr.td[1].a.GetRootParent()).InnerHtml.Trim();
            District      = rowNode.GetElementByTags(tr.td[2].a.GetRootParent())?.InnerHtml.Trim();
            LeaseNo      = rowNode.GetElementByTags(tr.td[3].table.tbody.tr.td[1].a.GetRootParent())?.InnerHtml.Trim();
            LeaseName    = rowNode.GetElementByTags(tr.td[4].GetRootParent())?.InnerHtml.Trim();
            WellNo       = rowNode.GetElementByTags(tr.td[5].GetRootParent())?.InnerHtml.Trim();
            FieldName    = rowNode.GetElementByTags(tr.td[6].a.GetRootParent())?.InnerHtml.Trim();
            OperatorName = rowNode.GetElementByTags(tr.td[7].a.GetRootParent())?.InnerHtml.Trim();
            County        = rowNode.GetElementByTags(tr.td[8].a.GetRootParent())?.InnerHtml.Trim();
            OnSchedule   = rowNode.GetElementByTags(tr.td[9].GetRootParent())?.InnerHtml.Trim();
            ApiDepth     = rowNode.GetElementByTags(tr.td[10].GetRootParent())?.InnerHtml.Trim();
        }

        public override string ToString()
        {
            return $"{ApiNo} {District} {LeaseNo} {LeaseName} {WellNo} {FieldName} {OperatorName} {County} {OnSchedule} {ApiDepth}";
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
}