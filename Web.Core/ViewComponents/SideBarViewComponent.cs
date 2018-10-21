namespace PrimeNumberCalculator.ViewComponents
{
    using System.Threading.Tasks;
    using Microsoft.AspNetCore.Mvc;
    using PrimeNumberCalculator.Model;

    public class SideBarViewComponent : ViewComponent
    {
        public async Task<IViewComponentResult> InvokeAsync()
        {
            var urls = await Task.FromResult(new []
            {
                new Link { Url = "snittlistan.se", Text = "Snittlistan" }
            });

            return View(urls);
        }
    }
}
