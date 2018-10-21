namespace PrimeNumberCalculator.TagHelpers
{
    using System.Threading.Tasks;
    using Microsoft.AspNetCore.Razor.TagHelpers;
    using Microsoft.Extensions.Logging;

    public enum TargetEnum
    {
        None,
        Blank,
        Parent,
        Self,
        Top
    }

    public class UrlTagHelper : TagHelper
    {
        public UrlTagHelper(ILogger<UrlTagHelper> logger)
        {
            logger.LogInformation("Inside the url tag helper");
        }

        public TargetEnum Target { get; set; }

        public override async Task ProcessAsync(TagHelperContext context, TagHelperOutput output)
        {
            output.TagName = "a";
            var content = await output.GetChildContentAsync();
            output.Attributes.SetAttribute("href", content.GetContent());
            if (Target != TargetEnum.None)
            {
                output.Attributes.SetAttribute("target", $"_{Target.ToString().ToLowerInvariant()}");
            }
        }
    }
}
