using System;
using System.Collections.Generic;
using System.Linq;
using System.IO;
using System.Windows.Forms;

namespace ConsoleApp1
{
    class Program
    {
        [STAThread]
        static void Main(string[] args)
        {
            string line = "";
            List<string> list_Temporary = new List<string>();
            List<string> user = new List<string>();
            List<string> list = new List<string>();
            List<string> textPostlist = new List<string>();
            using (var fbd = new FolderBrowserDialog())
            {
                DialogResult result = fbd.ShowDialog();

                if(result == DialogResult.OK && !string.IsNullOrWhiteSpace(fbd.SelectedPath))
                {
                    string[] files = Directory.GetFiles(fbd.SelectedPath);
                    for(int i=0; i< files.Length; i++)
                    {
                        using (StreamReader sr = new StreamReader(files[i]))
                        {
                            line = sr.ReadToEnd();
                            if (line.Contains("Product") || line.Contains("Offers"))
                            {
                                if(i != files.Length)
                                {
                                    list.Add(files[i].ToString());
                                    textPostlist.Add(line);
                                    Console.Write("\r{0}   ", "Reading files.");
                                }
                                else
                                {
                                    sr.Close();
                                }
                            }
                        }
                    }

                    findUsersAndPosts(user, textPostlist, list_Temporary);
                }
            }
        }

        public static void findUsersAndPosts(List<string> user, List<string> textPostlist, List<string> list_Temporary)
        {
            int locationExact; int locationDeriv; int lengthUsername; int post_Start; int post_End; List<string> post = new List<string>();
            list_Temporary = new List<string>();
            for (int i = 0; i < textPostlist.Count; i++)
            {
                string combinedText = "";
                list_Temporary = textPostlist[i].Split().ToList();
                for (int j = 0; j < list_Temporary.Count; j++)
                {
                    if (list_Temporary[j] == "Post" && list_Temporary[j + 1] == "by:")
                    {
                        locationExact = j + 2;
                        for (int k = 3; k < 8; k++)
                        {
                            if (list_Temporary[j + k] == "on")
                            {
                                post_Start = 0; post_End = 0;
                                locationDeriv = j + k;
                                lengthUsername = locationDeriv - locationExact;
                                username_Combiner(locationExact, locationDeriv, lengthUsername, user, combinedText, list_Temporary);
                                post_Start = postSelectorStart(locationExact, lengthUsername, post_Start, list_Temporary);
                                post_End = postSelectorEnd(locationDeriv, list_Temporary, post_End);
                                post_Finalizer(list_Temporary, post, post_Start, post_End);
                                break;
                            }
                        }
                    }
                }
            }

            int print = user.Count;
            using (TextWriter sw = new StreamWriter("C:\\Users\\Joeyh\\Desktop\\Uitkomst\\Uitkomst.csv"))
            {
                sw.WriteLine("{0};{1}", "Usernames", "Posts");
                for (int i = 0; i < print; i++)
                {
                    float progress = (i * 1.0f / print) * 100;
                    sw.WriteLine("{0};{1}", user[i], post[i]);
                    Console.Write("\r{0}   ", ((int)progress + 1) + " % of the posts are read. Press enter to close the program.");
                }
                Console.ReadLine();
            }
        }
        private static void username_Combiner(int locationExact, int locationDeriv, int lengthUsername, List<string> user, string combinedText, List<string> list_Temporary)
        {
            string[] names = new string[5];
            for (int i = locationDeriv; i > locationExact; i--)
            {
                if(lengthUsername == 1)
                {
                    combinedText = string.Join(" ", list_Temporary[locationExact], combinedText);
                    user.Add(combinedText);
                }
                else
                {
                    for (int j = 0; j < 5; j++)
                    {
                        if (list_Temporary[locationExact + j] != "on")
                        {
                            names[j] = string.Join(" ", list_Temporary[locationExact + j]);
                        }
                        else
                        {
                            user.Add (string.Join(" ", names));
                            lengthUsername = j;
                            break;
                        }
                    }
                    break;
                }
            }
        }

        private static int postSelectorStart(int locationExact, int lengthUsername, int post_Start, List<string> list_Temporary)
        {
            post_Start = locationExact + lengthUsername + 10;
            for (int i = post_Start; i < (post_Start + 15); i++)
            {
                for(int j = 0; j < 15; j++)
                {
                    try
                    {
                        if (list_Temporary[post_Start + j] == "Quote")
                        {
                            if(list_Temporary[post_Start + j + lengthUsername + 17] == "Quote")
                            {
                                return (post_Start + 2 * 18 + lengthUsername);
                            }
                            else
                            {
                                return (post_Start + 18 + lengthUsername);
                            }
                        }
                        else if (list_Temporary[post_Start + j] == "-----BEGIN" || list_Temporary[post_Start + j] == " -----BEGIN" || list_Temporary[post_Start + j] == "  -----BEGIN" || list_Temporary[post_Start + j] == "   -----BEGIN" || list_Temporary[post_Start + j] == "    -----BEGIN")
                        {
                            try
                            {
                                for (int k = 0; k < 200; k++)
                                {
                                    if (list_Temporary[post_Start + k] == "BLOCK-----")
                                    {
                                        for (int l = 0; l < 200; l++)
                                        {
                                            if (list_Temporary[(post_Start + k + l)] == "KEY" && list_Temporary[(post_Start + k + l + 1)] == "BLOCK-----")
                                            {
                                                return post_Start + k + l + 7;
                                            }
                                        }
                                    }
                                    else if (list_Temporary[post_Start + k] == "Hash:")
                                    {
                                        return post_Start + k + 7;
                                    }
                                }
                            }
                            catch
                            {
                                break;
                            }

                        }
                    }
                    catch
                    {
                        break;
                    }
                }
            }
            return post_Start = locationExact + lengthUsername + 10;
        }

        private static int postSelectorEnd(int locationDeriv, List<string> list_Temporary, int post_End)
        {
            for (int i = locationDeriv; i < list_Temporary.Count; i++)
            {
                if (list_Temporary[i] == "Title:" || ((list_Temporary[i] == "SMF") && (list_Temporary[i + 1] == "|") && (list_Temporary[i + 2] == "SMF") && (list_Temporary[i + 3] == "©")))
                {
                    post_End = i;
                    break;
                }
            }
            return post_End;
        }

        private static void post_Finalizer(List<string> list_Temporary, List<string> userPost, int post_Start, int post_End)
        {
            int teller = 0;
            int postLength = post_End - post_Start;
            string[] post = new string[postLength];

            for (int i = post_Start; i < post_End; i++)
            {
                post[teller] = string.Join(" ", list_Temporary[i]);
                teller++;
            }

            userPost.Add(string.Join(" ", post));
        }
    }
}
