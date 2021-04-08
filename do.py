import tkinter as tk
from tkinter import ttk
import tkinter.font as font
from PIL import ImageTk, Image
from subprocess import check_output
from os.path import isfile
from os import getcwd


"""
This function is set up to return the element symbol for a
given Z value, where Z is a positive integer up to 118
"""
def swap_element_label(z):
    elementlist = [[1, "H", "Hydrogen"], [2, "He", "Helium"], [3, "Li", "Lithium"], [4, "Be", "Beryllium"],
                   [5, "B", "Boron"], [6, "C", "Carbon"], [7, "N", "Nitrogen"], [8, "O", "Oxygen"],
                   [9, "F", "Fluorine"], [10, "Ne", "Neon"], [11, "Na", "Sodium"], [12, "Mg", "Magnesium"],
                   [13, "Al", "Aluminum"], [14, "Si", "Silicon"], [15, "P", "Phosphorus"], [16, "S", "Sulfur"],
                   [17, "Cl", "Chlorine"], [18, "Ar", "Argon"], [19, "K", "Potassium"], [20, "Ca", "Calcium"],
                   [21, "Sc", "Scandium"], [22, "Ti", "Titanium"], [23, "V", "Vanadium"], [24, "Cr", "Chromium"],
                   [25, "Mn", "Manganese"], [26, "Fe", "Iron"], [27, "Co", "Cobalt"], [28, "Ni", "Nickel"],
                   [29, "Cu", "Copper"], [30, "Zn", "Zinc"], [31, "Ga", "Gallium"], [32, "Ge", "Germanium"],
                   [33, "As", "Arsenic"], [34, "Se", "Selenium"], [35, "Br", "Bromine"], [36, "Kr", "Krypton"],
                   [37, "Rb", "Rubidium"], [38, "Sr", "Strontium"], [39, "Y", "Yttrium"], [40, "Zr", "Zirconium"],
                   [41, "Nb", "Niobium"], [42, "Mo", "Molybdenum"], [43, "Tc", "Technetium"], [44, "Ru", "Ruthenium"],
                   [45, "Rh", "Rhodium"], [46, "Pd", "Palladium"], [47, "Ag", "Silver"], [48, "Cd", "Cadmium"],
                   [49, "In", "Indium"], [50, "Sn", "Tin"], [51, "Sb", "Antimony"], [52, "Te", "Tellurium"],
                   [53, "I", "Iodine"], [54, "Xe", "Xenon"], [55, "Cs", "Cesium"], [56, "Ba", "Barium"],
                   [57, "La", "Lanthanum"], [58, "Ce", "Cerium"], [59, "Pr", "Praseodymium"], [60, "Nd", "Neodymium"],
                   [61, "Pm", "Promethium"], [62, "Sm", "Samarium"], [63, "Eu", "Europium"], [64, "Gd", "Gadolinium"],
                   [65, "Tb", "Terbium"], [66, "Dy", "Dysprosium"], [67, "Ho", "Holmium"], [68, "Er", "Erbium"],
                   [69, "Tm", "Thulium"], [70, "Yb", "Ytterbium"], [71, "Lu", "Lutetium"], [72, "Hf", "Hafnium"],
                   [73, "Ta", "Tantalum"], [74, "W", "Tungsten"], [75, "Re", "Rhenium"], [76, "Os", "Osmium"],
                   [77, "Ir", "Iridium"], [78, "Pt", "Platinum"], [79, "Au", "Gold"], [80, "Hg", "Mercury"],
                   [81, "Tl", "Thallium"], [82, "Pb", "Lead"], [83, "Bi", "Bismuth"], [84, "Po", "Polonium"],
                   [85, "At", "Astatine"], [86, "Rn", "Radon"], [87, "Fr", "Francium"], [88, "Ra", "Radium"],
                   [89, "Ac", "Actinium"], [90, "Th", "Thorium"], [91, "Pa", "Protactinium"], [92, "U", "Uranium"],
                   [93, "Np", "Neptunium"], [94, "Pu", "Plutonium"], [95, "Am", "Americium"], [96, "Cm", "Curium"],
                   [97, "Bk", "Berkelium"], [98, "Cf", "Californium"], [99, "Es", "Einsteinium"],
                   [100, "Fm", "Fermium"], [101, "Md", "Mendelevium"], [102, "No", "Nobelium"],
                   [103, "Lr", "Lawrencium"], [104, "Rf", "Rutherfordium"], [105, "Db", "Dubnium"],
                   [106, "Sg", "Seaborgium"], [107, "Bh", "Bohrium"], [108, "Hs", "Hassium"], [109, "Mt", "Meitnerium"],
                   [110, "Ds", "Darmstadtium"], [111, "Rg", "Roentgenium"], [112, "Cn", "Copernicium"],
                   [113, "Nh", "Nihonium"], [114, "Fl", "Flerovium"], [115, "Mc", "Moscovium"],
                   [116, "Lv", "Livermorium"], [117, "Ts", "Tennessine"], [118, "Og", "Oganesson"]]
    for row in elementlist:
        if str(row[0]) == str(z):
            return row[1]


"""
These two lines are used to ensure the correct path to
the fortran executables is used, could be removed with 
a different implementation. But I experienced errors 
without this addition.
"""
cwd = getcwd()
codespath = cwd +"\codes"


"""
window and tab_frame need to be created first, above
the function definitions so that the tabs can be
manipulated in the functions.
A reformatting of the GUI as a class would allow for 
a reordering of the code.
"""
window = tk.Tk()
tab_frame = ttk.Notebook(window)
window.title("Creating the Isotopes - PAN 2020")
active_list = [] # used to track which tabs have already been made



def InputErrorWindow(p):
    """
    A generic error window to be called when the input in
    Z or A can not be parsed correctly.
    """
    error_window = tk.Tk()
    error_window.title("Type Error")
    error_text_lbl = "Your input: " + str(p) + '\n\n'
    error_text_lbl += "Unable to interpret as an integer or range of integers.\n\n"
    error_text = tk.Label(text=error_text_lbl, master=error_window)
    error_text.pack()
    error_window.mainloop()



def TypeCheck(x):
    """
    This function takes in the user input for Z (or A)
    and splits it into a list of integer values.
    Should catch negative numbers, non-integer inputs.
    """
    if '-' in x and x[0] != '-':
        x_range = x.split('-')
        if len(x_range) > 2:
            InputErrorWindow(x)
            return
    else:
        if not x.isdigit():
            InputErrorWindow(x)
            return
        x_range = [int(x), int(x)]
    x_list = list(range(int(x_range[0]), int(x_range[1])+1))
    return x_list



def DecipherInput(protons, nucleons):
    proton_list = TypeCheck(protons)
    nucleons_list = TypeCheck(nucleons)
    for z in proton_list:
        for a in nucleons_list:
            if not isfile("results-density\\" + str(z).zfill(3) + '-' + str(a).zfill(3) + "-den.jpg"):
                with open("codes/answers.txt", 'w') as f:
                    line = str(z) + '  ' + str(a)
                    f.write(line)
                p = check_output("aaa < answers.txt", cwd=codespath, shell=True)
    for z in proton_list:
        for a in nucleons_list:
            MakeNewTab(z, a)

def MakeNewTab(Z, A):
    newtab = tk.Frame()
    file_name = str(Z).zfill(3) + '-' + str(A).zfill(3)
    tab_name = str(A) + '-' + swap_element_label(Z)
    for i in range(0, len(active_list)):
        if tab_name == active_list[i]:
            tab_frame.select(i)
            return
    active_list.append(tab_name)
    img_den = ImageTk.PhotoImage(Image.open("results-density\\" + file_name + "-den.jpg").resize((641, 526)))
    img_sps = ImageTk.PhotoImage(Image.open("results-levels\\" + file_name + "-sps.jpg").resize((566, 766)))
    den_panel = tk.Label(image=img_den, master=newtab)
    sps_panel = tk.Label(image=img_sps, master=newtab)
    # the next two lines seem redundant, but are needed to keep pictures in frame
    den_panel.image = img_den
    sps_panel.image = img_sps
    details_panel_lbl = 'The panel to the left shows the quantum orbits and their energies \n' \
                        'for protons and neutrons in ' + tab_name + '\n\n' \
                        'The figure above shows the radial densities of protons and neutrons.\n\n' \
                        'You can run multiple nuclei at once by entering a \n' \
                        'range of Z or A values (e.g. 16-20)'

    details_panel = tk.Label(text=details_panel_lbl, master=newtab, justify="left")
    details_panel['font'] = font.Font(size=15)
    details_panel.grid(column=1, row=1, sticky='N')
    den_panel.grid(column=1, row=0, padx=0, pady=0,  sticky='N')
    sps_panel.grid(column=0, row=0, rowspan=2, padx=0, pady=0)
    tab_frame.insert("end", newtab, text=tab_name)
    tab_frame.select(list(tab_frame.tabs())[-1])

    return

def DeleteTabs():
    for item in list(tab_frame.tabs()):
        tab_frame.select(item)
        tab_frame.forget(tab_frame.select())
    global active_list
    active_list = []
    DecipherInput('8', '16')


DecipherInput('8', '16')

header_frame = tk.Frame()

proton_entry_label = tk.Label(text="Number of Protons (Z)", master=header_frame)
proton_entry = tk.Entry(master=header_frame)
proton_entry.insert(0, '8')
proton_entry_label.grid(column=1, row=0, padx=10, pady=0, )
proton_entry.grid(column=1, row=1, padx=10, pady=0)

nucleon_entry_label = tk.Label(text="Number of Nucleons (A)", master=header_frame)
nucleon_entry = tk.Entry(master=header_frame)
nucleon_entry.insert(0, '16')
nucleon_entry_label.grid(column=2, row=0, padx=10, pady=0)
nucleon_entry.grid(column=2, row=1, padx=10, pady=0)

update_button = tk.Button(master=header_frame, text='Explore nucleus',
                          command=lambda: DecipherInput(proton_entry.get(), nucleon_entry.get()))
update_button.grid(column=3, row=1, padx=10, pady=0)

refresh_button = tk.Button(master=header_frame, text='Reset Tabs',
                          command=lambda: DeleteTabs())
refresh_button.grid(column=5, row=1, padx=10, pady=0)

header_frame.pack()
tab_frame.pack(expand=1, fill="both")
window.iconbitmap('nucleus.ico')
window.mainloop()
