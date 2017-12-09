#!/usr/bin/env python

import sys
import os
import re
import glob
import shutil
import argparse

import common

log = common.get_log()

# Global paths

base_path = os.path.dirname(os.path.normpath(sys.argv[0]))
output_path = os.path.join(base_path, "images")
plot_tmp = os.path.join(base_path, "tmp_plot")
plot_cache = os.path.join(base_path, "cache_plot")

fptaylor_base = os.path.normpath(os.path.join(base_path, ".."))
fptaylor_tmp = os.path.join(base_path, "tmp_fptaylor")
fptaylor_log = os.path.join(base_path, "log_fptaylor")
fptaylor = os.path.join(fptaylor_base, "fptaylor")

error_bounds_path = os.path.normpath(
    os.path.join(base_path, "..", "..", "ErrorBounds"))
racket_plot = os.path.join(error_bounds_path, "racket", "plot-fptaylor.rkt")
racket = "racket"


def files_from_template(fname_template):
    result = {}
    if not fname_template:
        return result
    pat = re.sub(r"\\{task\\}", r"(.*)", re.escape(fname_template))
    log.debug("pat = {0}".format(pat))
    for fname in glob.glob(re.sub("{task}", "*", fname_template)):
        if os.path.isfile(fname):
            log.debug("fname = {0}".format(fname))
            m = re.match(pat, fname)
            task = m.group(1)
            result[task] = fname
    return result

# Parse arguments

parser = argparse.ArgumentParser(
    description="Runs FPTaylor with different configurations and plots error model functions.")

parser.add_argument('--debug', action='store_true',
                    help="debug mode")

parser.add_argument('-c', '--config', action='append', nargs='+',
                    help="add a configuration file (or several files)")

parser.add_argument('-e', '--error', choices=['abs', 'rel', 'ulp'], default='abs',
                    help="error type (overrides error types defined in configuration files)")

parser.add_argument('-t', '--type', default='64',
                    choices=['16', '32', '64', 'real'], 
                    help="default type of variables and rounding operations.\
                          Also controls flags of ErrorBounds.")

parser.add_argument('-r', '--range',
                    help="redefine the range of input variables")

parser.add_argument('-v', '--verbosity', type=int, default=1,
                    help="FPTaylor's verbosity level")

parser.add_argument('-s', '--samples', type=int, default=1000,
                    help="number of sample points (intervals) for plots")

parser.add_argument('--approx-plot', action='store_true',
                    help="produce approximate plots of FPTaylor error models")

parser.add_argument('--segments', type=int, default=500,
                    help="number of segments for ErrorBounds")

parser.add_argument('--err-samples', type=int, default=10000,
                    help="number of samples for ErrorBounds")

parser.add_argument('--data-plot-style', choices=['rectangles', 'lines'],
                    default='rectangles',
                    help="specifies how to plot ErrorBounds results")

parser.add_argument('--update-cache', action='store_true',
                    help="do not use cached files")

parser.add_argument('input', nargs='+',
                    help="input FPTaylor files")

args = parser.parse_args()

if args.debug:
    log.setLevel(logging.DEBUG)

if not args.config:
    args.config = [None]

log.debug("tmp_dir = {0}".format(plot_tmp))
log.debug("cache_dir = {0}\n".format(plot_cache))

if not os.path.isdir(output_path):
    os.makedirs(output_path)
if not os.path.isdir(plot_tmp):
    os.makedirs(plot_tmp)
if not os.path.isdir(plot_cache):
    os.makedirs(plot_cache)

common.remove_all(plot_tmp, "*")
common.remove_all(fptaylor_tmp, "*")


def basename(fname):
    return os.path.splitext(os.path.basename(fname))[0]


def restrict_input_vars(fname, range):
    ns = [s.strip() for s in range.split(",")]
    if len(ns) == 1 and ns[0]:
        repl = r"[{0}, \2]".format(ns[0])
    elif len(ns) == 2:
        repl = "["
        repl += ns[0] if ns[0] else r"\1"
        repl += ", "
        repl += ns[1] if ns[1] else r"\2"
        repl += "]"
    else:
        return
    common.replace_in_file(fname, [(r"[\w]+[\s]+in[\s]*\[.+,.+\]",
                                    r"\[(.+),(.+)\]",
                                    repl)])


def run_error_bounds(input_file):
    exe_file = os.path.join(plot_tmp, "a.out")
    out_file = os.path.join(plot_tmp, basename(input_file) + "-data.txt")
    common.remove_files([exe_file, out_file])

    src_files = [os.path.join(error_bounds_path, f) for f in
                 ["search_mpfr.c", "search_mpfr_main.c", "search_mpfr_utils.c"]]

    compile_cmd = ["gcc", "-o", exe_file, "-O3",
                   "-std=c99", "-I" + error_bounds_path]
    compile_cmd += src_files + [input_file]
    compile_cmd += ["-lmpfr", "-lgmp"]

    cmd = [exe_file,
           "-n", str(args.segments),
           "-s", str(args.err_samples)]

    if args.type == "32":
        cmd += ["-f"]
    elif args.type == "real":
        cmd += ["-r"]

    cached_file = common.find_in_cache(plot_cache, input_file, cmd)
    if cached_file and not args.update_cache:
        log.info("A cached ErrorBounds result is found")
        shutil.copy(cached_file, out_file)
        return out_file

    common.run(compile_cmd, log=log)
    common.run(cmd + ["-o", out_file], log=log)

    common.cache_file(plot_cache, out_file, input_file, cmd)
    return out_file


# Run FPTaylor for each input file

class PlotTask:
    def __init__(self, base_name):
        self.base_name = base_name
        self.racket_files = []
        self.data_files = []

    def plot(self):
        # Run plot-fptaylor.rkt
        image_name = self.base_name
        if args.type:
            image_name += "-" + args.type
        if args.approx_plot:
            image_name += "-approx"
        image_file = os.path.join(output_path, image_name + ".png")

        cmd = [racket, racket_plot,
               "--out", image_file,
               "--samples", str(args.samples),
               "--data-style", args.data_plot_style]
        if args.approx_plot:
            cmd += ["--approx"]
        if self.data_files:
            cmd += ["--data"] + self.data_files
            cmd += ["--err-type", args.error]
        cmd += ["--"] + self.racket_files

        common.run(cmd, log=log)


class FPTaylorTask:
    def __init__(self, input_files):
        self.cfg_files = []
        if isinstance(input_files, list):
            self.input_files = list(input_files)
        else:
            self.input_files = [input_files]

        self.extra_args = [
            "-v", str(args.verbosity),
            "--opt-approx", "false",
            "--opt-exact", "true",
            "--tmp-base-dir", fptaylor_tmp,
            "--tmp-date", "false",
            "--log-base-dir", fptaylor_log,
            "--log-append-date", "none"
        ]

        if args.type:
            rnd_types = {
                "16": ("float16", "rnd16"),
                "32": ("float32", "rnd32"), 
                "64": ("float64", "rnd64"),
                "real": ("real", "rnd64")
            }
            var_type, rnd_type = rnd_types[args.type]
            self.extra_args += ["--default-var-type", var_type]
            self.extra_args += ["--default-rnd", rnd_type]

        if args.error == 'abs':
            self.extra_args += ["-abs", "true", "-rel", "false", "-ulp", "false"]
        elif args.error == 'rel':
            self.extra_args += ["-abs", "false", "-rel", "true", "-ulp", "false"]
        else:
            self.extra_args += ["-abs", "false", "-rel", "false", "-ulp", "true"]

    def run(self, args):
        cfg_args = []
        for cfg in self.cfg_files:
            cfg_args += ["-c", cfg]
        cmd = [fptaylor] + self.input_files + cfg_args + args + self.extra_args
        common.run(cmd, log=log)


for fname in args.input:
    if not os.path.isfile(fname):
        log.error("Input file does not exist: {0}".format(fname))
        sys.exit(1)
    shutil.copy(fname, plot_tmp)
    fname = os.path.join(plot_tmp, os.path.basename(fname))
    base_fname = basename(fname) + "-" + args.error
    if args.range:
        restrict_input_vars(fname, args.range)
        base_fname += "-range"

    error_bounds_file_template = None
    common.remove_all(plot_tmp, base_fname + "*")
    
    plot_tasks = dict()

    for cfg_files in args.config:
        fptaylor_task = FPTaylorTask(fname)

        if not cfg_files:
            # default config
            cfg_name = "default"
        else:
            cfg_name = "-".join([basename(cfg) for cfg in cfg_files])
            for cfg_file in cfg_files:
                if not os.path.isfile(cfg_file):
                    log.error(
                        "Configuration file does not exist: {0}".format(cfg_file))
                    sys.exit(1)
                fptaylor_task.cfg_files.append(cfg_file)

        export_args = []

        if not error_bounds_file_template:
            error_bounds_file_template = os.path.join(plot_tmp, base_fname + "-{task}.c")
            export_args += ["--export-error-bounds", error_bounds_file_template]

        racket_file_template = os.path.join(
            plot_tmp, base_fname + "-" + cfg_name + "-{task}.rkt")
        export_args += ["--export-racket", racket_file_template]

        fptaylor_task.run(export_args)

        for task, racket_file in files_from_template(racket_file_template).iteritems():
            # Adjust the name in the output Racket files
            common.replace_in_file(racket_file,
                                   [(r"\(define name", '"([^"]*)"', r'"\1-{0}"'.format(cfg_name))])
            if task not in plot_tasks:
                plot_tasks[task] = PlotTask("[{0}]{1}".format(task, base_fname))
            plot_tasks[task].racket_files.append(racket_file)

    # ErrorBounds
    for task, input_file in files_from_template(error_bounds_file_template).iteritems(): 
        data_file = run_error_bounds(input_file)
        if task not in plot_tasks:
            log.warning("Undefined task '{0}' for the data file '{1}'".format(task, input_file))
            plot_tasks[task] = PlotTask("[{0}]{1}".format(task, base_fname))
        plot_tasks[task].data_files.append(data_file)

    # plot-fptaylor.rkt
    for task in plot_tasks.itervalues():
        task.plot()

