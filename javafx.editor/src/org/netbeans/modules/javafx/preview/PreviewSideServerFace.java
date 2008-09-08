/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.preview;

import java.rmi.*;

public interface PreviewSideServerFace extends Remote {
    public void run(Object context)  throws RemoteException;
    public void moveToFront()  throws RemoteException;
    public void notifyClassPathChanged()  throws RemoteException;
    public SerializableImage getPicture()  throws RemoteException;
}
