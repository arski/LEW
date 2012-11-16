Language Evolution Workbench

This is a quick starting guide for the Language Evolution Workbench.

The LEW is meant to simulate the evolution of a lexicon in a population of artificial agents that know very little about the world and even less about language.

There are almost no limits to what population and learning configurations can be simulated with the LEW, meaning that you have to adjust the setting of the model to your requirements when executing experiments. In particular, you can set how many agents will be active, their chances of acting as speakers and selecting particular other agents as hearers, their learning strategies and much much more.

All of the settings of the model are contained within the batch_run.pl file, with the different options shortly documented there as well (though admittedly _very_ shortly - for more details please refer to the parameters section of my PhD for the time being).

When you are happy with the settings of the LEW, all you need to do is set the X inside the "runbatch(X)." line at the end of batch_run.pl to the number of randomly seeded experiments that you want to perform and you're ready to go.

To start the model, it is suggested you use:

nohup sicstus -l batch_run.pl > outfile.out &

This will start the simulation in the background and let you do other stuff. Of course, you can also keep the simulation in your console by omitting the "nohup" command (and then ideally turning the print trace setting on as well, otherwise you won't see much).

The above command will perform the experiment and write data to a timestamped folder inside the data/ folder of your SVN/git checkout of the project. Once all experiments have completed, please refer to the analysis section in src/analysis/ for further steps.

The authors can provide no guarantee regarding the correctness of the model, especially not within the greater scheme of the language evolution dilemma. In particular, make sure to understand and write down all of the parameterized assumptions before/when drawing any conclusions from the obtained data.

In case of questions, feel free to contact {vogel,bachwerm}@cs.tcd.ie.
