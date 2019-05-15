namespace PrimeNumberCalculator.ViewComponents
{
    using System.Threading.Tasks;
    using Microsoft.AspNetCore.Mvc;
    using Microsoft.Extensions.Logging;
    using PrimeNumberCalculator.Model;

    public class SideBar : ViewComponent
    {
        private readonly ILogger<SideBar> logger;

        public SideBar(ILogger<SideBar> logger)
        {
            this.logger = logger;
        }

        public async Task<IViewComponentResult> InvokeAsync(string source)
        {
            logger.LogInformation("InvokeAsync");
            var urls = await Task.FromResult(new []
            {
                new Link { Url = "https://snittlistan.se", Text = $"Snittlistan (source={source})" }
            });

            return View(urls);
        }
    }
}
