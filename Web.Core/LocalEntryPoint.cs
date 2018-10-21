namespace Web.Core
{
    using System;
    using Microsoft.AspNetCore;
    using Microsoft.AspNetCore.Hosting;
    using Microsoft.Extensions.DependencyInjection;
    using Microsoft.Extensions.Logging;

    public class Program
    {
        public static int Main(string[] args)
        {
            try
            {
                var host = WebHost.CreateDefaultBuilder(args)
                    .UseStartup<Startup>()
                    .Build();
                var logger = host.Services.GetRequiredService<ILogger<Program>>();
                logger.LogInformation("Starting application");
                host.Run();
                return 0;
            }
            catch (Exception ex)
            {
                System.Console.WriteLine(ex);
                return 1;
            }
        }
    }
}
