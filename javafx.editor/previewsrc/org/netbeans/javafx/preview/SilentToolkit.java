package org.netbeans.javafx.preview;

import java.awt.Button;
import java.awt.Canvas;
import java.awt.Checkbox;
import java.awt.CheckboxMenuItem;
import java.awt.Choice;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.FileDialog;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Frame;
import java.awt.HeadlessException;
import java.awt.Image;
import java.awt.Label;
import java.awt.List;
import java.awt.Menu;
import java.awt.MenuBar;
import java.awt.MenuItem;
import java.awt.Panel;
import java.awt.PopupMenu;
import java.awt.PrintJob;
import java.awt.ScrollPane;
import java.awt.Scrollbar;
import java.awt.TextArea;
import java.awt.TextField;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.datatransfer.Clipboard;
import java.awt.dnd.DragGestureEvent;
import java.awt.dnd.InvalidDnDOperationException;
import java.awt.dnd.peer.DragSourceContextPeer;
import java.awt.font.TextAttribute;
import java.awt.im.InputMethodHighlight;
import java.awt.image.ColorModel;
import java.awt.image.ImageObserver;
import java.awt.image.ImageProducer;
import java.awt.peer.ButtonPeer;
import java.awt.peer.CanvasPeer;
import java.awt.peer.CheckboxMenuItemPeer;
import java.awt.peer.CheckboxPeer;
import java.awt.peer.ChoicePeer;
import java.awt.peer.DialogPeer;
import java.awt.peer.FileDialogPeer;
import java.awt.peer.FontPeer;
import java.awt.peer.FramePeer;
import java.awt.peer.LabelPeer;
import java.awt.peer.ListPeer;
import java.awt.peer.MenuBarPeer;
import java.awt.peer.MenuItemPeer;
import java.awt.peer.MenuPeer;
import java.awt.peer.PanelPeer;
import java.awt.peer.PopupMenuPeer;
import java.awt.peer.ScrollPanePeer;
import java.awt.peer.ScrollbarPeer;
import java.awt.peer.TextAreaPeer;
import java.awt.peer.TextFieldPeer;
import java.awt.peer.WindowPeer;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URL;
import java.util.Map;
import java.util.Properties;

/**
 *
 * @author Adam
 */
public class SilentToolkit extends Toolkit {
    
    static Toolkit delegate;

    static Method createButton, createTextField, createLabel, createList, createCheckbox, createScrollbar, createScrollPane,
                  createTextArea, createChoice, createFrame, createCanvas, createPanel, createWindow, createDialog, createMenuBar, createMenu,
                  createPopupMenu, createMenuItem, createFileDialog, createCheckboxMenuItem, getFontPeer, getSystemEventQueueImpl;
    {   
        try {
            createButton = Toolkit.class.getDeclaredMethod("createButton", Button.class);
            createTextField = Toolkit.class.getDeclaredMethod("createTextField", TextField.class);
            createLabel = Toolkit.class.getDeclaredMethod("createLabel", Label.class);
            createList = Toolkit.class.getDeclaredMethod("createList", List.class);
            createCheckbox = Toolkit.class.getDeclaredMethod("createCheckbox", Checkbox.class);
            createScrollbar = Toolkit.class.getDeclaredMethod("createScrollbar", Scrollbar.class);
            createScrollPane = Toolkit.class.getDeclaredMethod("createScrollPane", ScrollPane.class);
            createTextArea = Toolkit.class.getDeclaredMethod("createTextArea", TextArea.class);
            createChoice = Toolkit.class.getDeclaredMethod("createChoice", Choice.class);
            createFrame = Toolkit.class.getDeclaredMethod("createFrame", Frame.class);
            createCanvas = Toolkit.class.getDeclaredMethod("createCanvas", Canvas.class);
            createPanel = Toolkit.class.getDeclaredMethod("createPanel", Panel.class);
            createWindow = Toolkit.class.getDeclaredMethod("createWindow", Window.class);
            createDialog = Toolkit.class.getDeclaredMethod("createDialog", Dialog.class);
            createMenuBar = Toolkit.class.getDeclaredMethod("createMenuBar", MenuBar.class);
            createMenu = Toolkit.class.getDeclaredMethod("createMenu", Menu.class);
            createPopupMenu = Toolkit.class.getDeclaredMethod("createPopupMenu", PopupMenu.class);
            createMenuItem = Toolkit.class.getDeclaredMethod("createMenuItem", MenuItem.class);
            createFileDialog = Toolkit.class.getDeclaredMethod("createFileDialog", FileDialog.class);
            createCheckboxMenuItem = Toolkit.class.getDeclaredMethod("createCheckboxMenuItem", CheckboxMenuItem.class);
            getFontPeer = Toolkit.class.getDeclaredMethod("getFontPeer", String.class, Integer.TYPE);
            getSystemEventQueueImpl = Toolkit.class.getDeclaredMethod("getSystemEventQueueImpl");
            createButton.setAccessible(true);
            createTextField.setAccessible(true);
            createLabel.setAccessible(true);
            createList.setAccessible(true); 
            createCheckbox.setAccessible(true);
            createScrollbar.setAccessible(true);
            createScrollPane.setAccessible(true);
            createTextArea.setAccessible(true);
            createChoice.setAccessible(true);
            createFrame.setAccessible(true);
            createCanvas.setAccessible(true);
            createPanel.setAccessible(true);
            createWindow.setAccessible(true);
            createDialog.setAccessible(true);
            createMenuBar.setAccessible(true);
            createMenu.setAccessible(true);
            createPopupMenu.setAccessible(true);
            createMenuItem.setAccessible(true);
            createFileDialog.setAccessible(true);
            createCheckboxMenuItem.setAccessible(true);
            getFontPeer.setAccessible(true);
            getSystemEventQueueImpl.setAccessible(true);
        } catch (NoSuchMethodException nsme) {
            nsme.printStackTrace();
        }
    }

    static private Object invoke(Method m, Object ... args) {
        try {
            return m.invoke(delegate, args);
        } catch (IllegalAccessException ex) {
            throw new Error(ex);
        } catch (IllegalArgumentException ex) {
            throw new Error(ex);
        } catch (InvocationTargetException ex) {
            Throwable t = ex.getCause();
            if (t instanceof RuntimeException) throw (RuntimeException)t;
            if (t instanceof Error) throw (Error)t;
            throw new Error(t);
        }
   }

    @Override
    protected ButtonPeer createButton(Button target) throws HeadlessException {
        return (ButtonPeer) invoke(createButton, target);
    }

    @Override
    protected TextFieldPeer createTextField(TextField target) throws HeadlessException {
        return (TextFieldPeer) invoke(createTextField, target);
    }

    @Override
    protected LabelPeer createLabel(Label target) throws HeadlessException {
        return (LabelPeer) invoke(createLabel, target);
    }

    @Override
    protected ListPeer createList(List target) throws HeadlessException {
        return (ListPeer) invoke(createList, target);
    }

    @Override
    protected CheckboxPeer createCheckbox(Checkbox target) throws HeadlessException {
        return (CheckboxPeer) invoke(createCheckbox, target);
    }

    @Override
    protected ScrollbarPeer createScrollbar(Scrollbar target) throws HeadlessException {
        return (ScrollbarPeer) invoke(createScrollbar, target);
    }

    @Override
    protected ScrollPanePeer createScrollPane(ScrollPane target) throws HeadlessException {
        return (ScrollPanePeer) invoke(createScrollPane, target);
    }

    @Override
    protected TextAreaPeer createTextArea(TextArea target) throws HeadlessException {
        return (TextAreaPeer) invoke(createTextArea, target);
    }

    @Override
    protected ChoicePeer createChoice(Choice target) throws HeadlessException {
        return (ChoicePeer) invoke(createChoice, target);
    }

    @Override
    protected FramePeer createFrame(Frame target) throws HeadlessException {
        return new SilentFramePeer((FramePeer) invoke(createFrame, target));
    }

    @Override
    protected CanvasPeer createCanvas(Canvas target) {
        return (CanvasPeer) invoke(createCanvas, target);
    }

    @Override
    protected PanelPeer createPanel(Panel target) {
        return (PanelPeer) invoke(createPanel, target);
    }

    @Override
    protected WindowPeer createWindow(Window target) throws HeadlessException {
        return new SilentWindowPeer((WindowPeer) invoke(createWindow, target));
    }

    @Override
    protected DialogPeer createDialog(Dialog target) throws HeadlessException {
        return new SilentDialogPeer((DialogPeer) invoke(createDialog, target));
    }

    @Override
    protected MenuBarPeer createMenuBar(MenuBar target) throws HeadlessException {
        return (MenuBarPeer) invoke(createMenuBar, target);
    }

    @Override
    protected MenuPeer createMenu(Menu target) throws HeadlessException {
        return (MenuPeer) invoke(createMenu, target);
    }

    @Override
    protected PopupMenuPeer createPopupMenu(PopupMenu target) throws HeadlessException {
        return (PopupMenuPeer) invoke(createPopupMenu, target);
    }

    @Override
    protected MenuItemPeer createMenuItem(MenuItem target) throws HeadlessException {
        return (MenuItemPeer) invoke(createMenuItem, target);
    }

    @Override
    protected FileDialogPeer createFileDialog(FileDialog target) throws HeadlessException {
        return new SilentFileDialogPeer((FileDialogPeer) invoke(createFileDialog, target));
    }

    @Override
    protected CheckboxMenuItemPeer createCheckboxMenuItem(CheckboxMenuItem target) throws HeadlessException {
        return (CheckboxMenuItemPeer) invoke(createCheckboxMenuItem, target);
    }

    @Override
    protected FontPeer getFontPeer(String name, int style) {
        return (FontPeer) invoke(getFontPeer, name, style);
    }

    @Override
    public Dimension getScreenSize() throws HeadlessException {
        return delegate.getScreenSize();
    }

    @Override
    public int getScreenResolution() throws HeadlessException {
        return delegate.getScreenResolution();
    }

    @Override
    public ColorModel getColorModel() throws HeadlessException {
        return delegate.getColorModel();
    }

    @Override
    public String[] getFontList() {
        return delegate.getFontList();
    }

    @Override
    public FontMetrics getFontMetrics(Font font) {
        return delegate.getFontMetrics(font);
    }

    @Override
    public void sync() {
        delegate.sync();
    }

    @Override
    public Image getImage(String filename) {
        return delegate.getImage(filename);
    }

    @Override
    public Image getImage(URL url) {
        return delegate.getImage(url);
    }

    @Override
    public Image createImage(String filename) {
        return delegate.createImage(filename);
    }

    @Override
    public Image createImage(URL url) {
        return delegate.createImage(url);
    }

    @Override
    public boolean prepareImage(Image image, int width, int height, ImageObserver observer) {
        return delegate.prepareImage(image, width, height, observer);
    }

    @Override
    public int checkImage(Image image, int width, int height, ImageObserver observer) {
        return delegate.checkImage(image, width, height, observer);
    }

    @Override
    public Image createImage(ImageProducer producer) {
        return delegate.createImage(producer);
    }

    @Override
    public Image createImage(byte[] imagedata, int imageoffset, int imagelength) {
        return delegate.createImage(imagedata, imageoffset, imagelength);
    }

    @Override
    public PrintJob getPrintJob(Frame frame, String jobtitle, Properties props) {
        return delegate.getPrintJob(frame, jobtitle, props);
    }

    @Override
    public void beep() {
//        delegate.beep();
    }

    @Override
    public Clipboard getSystemClipboard() throws HeadlessException {
        return delegate.getSystemClipboard();
    }

    @Override
    protected EventQueue getSystemEventQueueImpl() {
        return (EventQueue) invoke(getSystemEventQueueImpl);
    }

    @Override
    public DragSourceContextPeer createDragSourceContextPeer(DragGestureEvent dge) throws InvalidDnDOperationException {
        return delegate.createDragSourceContextPeer(dge);
    }

    @Override
    public Map<TextAttribute, ?> mapInputMethodHighlight(InputMethodHighlight highlight) throws HeadlessException {
        return delegate.mapInputMethodHighlight(highlight);
    }

}
