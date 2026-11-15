package fiuba.algo3.tp1;

import java.util.ArrayList;
import java.util.List;

public class Grupo {

	private String nombre;
	private String administrador;
	private List<Contacto> contactos;
	private List<String> conversacion;
	
	private Contacto getContacto(String nombre) {
		for (Contacto contacto : contactos) {
			if (contacto.getNombre().equals(nombre)) {
				return contacto;
			}
		}
		return null;
	}
	
	public Grupo(String nombre, String administrador) {
		setNombre(nombre);
		setAdministrador(administrador);
		contactos = new ArrayList<Contacto>();
		contactos.add(new Contacto(administrador));
		conversacion = new ArrayList<String>();
		conversacion.add(0, "Conversacion");		
	}

	public String getNombre() {
		return nombre;
	}

	public void setNombre(String nombre) {
		this.nombre = nombre;
	}
	
	public String getAdministrador() {
		return administrador;
	}

	public void setAdministrador(String administrador) {
		this.administrador = administrador;
	}

	public int cantidadMiembros() {
		return contactos.size();
	}

	public void agregarContacto(String nombreContacto) {
		contactos.add(new Contacto(nombreContacto));
	}

	public int cantidadTotalMensajesRecibidos() {
		int mensajesRecibidos = 0;
		for (Contacto contacto : contactos) {
			mensajesRecibidos += contacto.cantidadTotalMensajesRecibidos();
		}
		return mensajesRecibidos;
	}

	public void recibirMensajeDe(String nombreContacto, String mensaje) {
		getContacto(nombreContacto).recibirMensaje(mensaje);
		conversacion.add(1, nombreContacto + ": " + mensaje);		
	}

	public int cantidadMensajesRecibidosDe(String nombreContacto) {
		return getContacto(nombreContacto).cantidadTotalMensajesRecibidos();
	}

	public void borrarMensajes() {
		for (Contacto contacto : contactos) {
			contacto.borrarMensajes();
		}
	}

	public int cantidadTotalMensajesEnviados() {
		int mensajesEnviados = 0;
		for (Contacto contacto : contactos) {
			mensajesEnviados += contacto.cantidadTotalMensajesEnviados();
		}
		return mensajesEnviados;
	}

	public void enviarMensaje(String mensaje) {
		getContacto(administrador).enviarMensaje(mensaje);
		conversacion.add(1, "Yo: " + mensaje);				
	}

	public List<String> obtenerConversacion() {
		return conversacion;
	}

}
