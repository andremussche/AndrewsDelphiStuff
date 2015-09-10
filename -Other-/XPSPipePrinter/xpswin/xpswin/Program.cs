using System;
using System.Windows.Forms;
using System.Printing;

namespace xpswin
{
    static class Program
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]          
        static void Main(string[] args)
        {
            try
            {
                Application.EnableVisualStyles();
                Application.SetCompatibleTextRenderingDefault(false);
            }
            catch (Exception e)
            {
                Console.WriteLine("{0} Exception caught during startup", e);
                MessageBox.Show(String.Format("Exception caught during startup: {0}", e));
            }

            if (args.Length != 2)
            {
              Console.WriteLine("Usage: <printername> <xps file>");
              MessageBox.Show("Usage: <printername> <xps file>");
            }
            else
            {
                try
                {
                    string sPrinter = args[0];
                    string xpsDocPath = args[1]; // "c:\\test\\test.xps";

                    LocalPrintServer localPrintServer = new LocalPrintServer();
                    PrintQueue defaultPrintQueue = localPrintServer.GetPrintQueue(sPrinter);
                    if (defaultPrintQueue == null)
                    {
                        Console.WriteLine("Printer not found: " + sPrinter);
                        MessageBox.Show("Printer not found: " + sPrinter);
                    }
                    else
                    {
                        PrintSystemJobInfo xpsPrintJob = defaultPrintQueue.AddJob("Print", xpsDocPath, false);
                    }
                }
                catch (Exception e)
                {
                    Console.WriteLine("{0} Exception caught.", e);
                    MessageBox.Show(String.Format("Exception caught during printing: {0}", e));
                }
            }
            
            //Application.Run(new Form1());
        }
    }
}
